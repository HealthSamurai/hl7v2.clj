(ns hl7v2.core
  (:require [clojure.string :as str]
            [hl7v2.schema.core :as schema]
            [hl7v2.schema.parsec :as parsec])
  (:import [java.util.regex Pattern]))



(defn pre-condigion [msg]
  (cond-> []
    (not (str/starts-with? msg "MSH")) (conj "Message should start with MSH segment")
    (< (.length msg) 8) (conj "Message is too short (MSH truncated)")))

(defn separators [msg]
  {:segment #"(\r|\n)"
   :field (get msg 3)
   :component (get msg 4)
   :subcomponet (get msg 7)
   :repetition (get msg 5)
   :escape (get msg 6)})

(defn split-by [s sep]
  (if (= java.util.regex.Pattern (type sep))
    (str/split s sep)
    (str/split s (re-pattern (Pattern/quote (str sep))))))

(defn indexed-map [coll]
  (loop [[x & xs] coll acc {} idx 1]
    (if (empty? xs)
      (if (nil? x) acc (assoc acc idx x))
      (recur xs
             (if (nil? x) acc (assoc acc idx x))
             (inc idx)))))

(defn parse-value [{sch :schema sep :separators :as ctx} tp v]
  (if (get schema/primitives (keyword (:type tp)))
    v
    (if-let [sub-tp (get-in sch [:types (keyword (:type tp))])]
      (let [sub-cmps (split-by v (:subcomponet sep))
            sub-types (:components sub-tp)]
        (loop [[c & cs] sub-cmps
               [s & ss] sub-types
               res {}]
          (let [res (if-not (str/blank? c) (assoc res (keyword (:key s)) (parse-value ctx s c)) res)]
            (if (empty? cs)
              res
              (recur cs ss res)))))
      (do 
        (println "WARN:" (pr-str (merge {} tp) v))
        v))))

(defn parse-component [ctx tp v]
  (if (:components tp)
    (let [cmps (split-by v (get-in ctx [:separators :component]))]
      (loop [[c & cs] cmps
             [s & ss] (:components tp)
             res {}]
        (let [res (if-not (str/blank? c) (assoc res (keyword (:key s)) (parse-value ctx s c)) res)]
          (if (empty? cs)
            res
            (recur cs ss res)))))
    v))

(defn parse-field [{sch :schema seps :separators :as ctx} {tpn :type c? :coll v :value :as f}]
  (let [tp (get-in sch [:types (keyword tpn)])
        vv (if c?
             (->> (split-by v (:repetition seps))
                  (mapv #(parse-component ctx tp %)))
             (parse-component ctx tp v))]
    vv))

(defn parse-segment [{sch :schema seps :separators :as ctx} seg]
  (let [fields (split-by seg (:field seps))
        [seg-name & fields] fields
        fields (if (= "MSH" seg-name)
                 (into ["|"] fields)
                 fields)

        seg-sch (get-in sch [:segments (keyword seg-name)])]
    [seg-name
     (loop [[f & fs] fields
            [s & ss] seg-sch
            acc {}]
       (let [s (merge s (get-in sch [:fields (keyword (:field s))]))]
         (if (str/blank? f)
           (recur fs ss acc)
           (let [acc  (assoc acc (keyword (:key s)) (parse-field ctx (assoc (or s {}) :value f)))]
             (if (empty? fs)
               acc
               (recur fs ss acc))))))]))

(defn parse [msg opts]
  (let [errs (pre-condigion msg)]
    (when-not (empty? errs)
      (throw (Exception. (str/join "; " errs))))
    (let [sch (schema/schema)
          seps (separators msg)
          ctx {:separators seps
               :schema sch}
          segments (->> (split-by msg (:segment seps))
                        (mapv str/trim)
                        (filter #(> (.length %) 0))
                        (mapv #(parse-segment ctx %)))
          {c :code e :event} (get-in segments [0 1 :type])
          grammar (get-in sch [:messages (keyword (str c "_" e))])]
      (println "GR"  (keyword (str c "_" e)) grammar)
      (parsec/parse grammar (mapv #(keyword (first %)) segments) (fn [idx] (get-in segments [idx 1]))))))
