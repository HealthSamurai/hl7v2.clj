(ns hl7v2.segment
  (:require [clj-yaml.core]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.regex Pattern]))


(defn load-yaml [nm]
  (if-let [f (io/resource nm)] 
    (clj-yaml.core/parse-string (slurp f) :keywords true)
    (throw (Exception. (str nm " not found")))))

(def metadata (load-yaml "seg2.yaml"))
(def types (load-yaml "types.yaml"))

(defn separators [msg]
  {:segment #"(\r\n|\r|\n)"
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

(defn not-empty? [v]
  (if (or (map? v) (sequential? v))
    (not (empty? v))
    (if (string? v)
      (not (str/blank? v))
      (not (nil? v)))))
(def primitives
  {:DT true
   :DTM true
   :FT true
   :GTS true
   :ID true
   :IS true
   :NM true
   :NUL true
   :SI true
   :HD true ;; hack
   :ST true
   :TM true
   :TX true
   :escapeType true
   :varies true})

(defn parse-value [sep tp v]
  (if (get primitives (keyword (:type tp)))
    v
    (if-let [sub-tp (get types (keyword (:type tp)))]
      (let [sub-cmps (split-by v (:subcomponet sep))
            sub-types (:components sub-tp)]
        (loop [[c & cs] sub-cmps
               [s & ss] sub-types
               res {}]
          (let [res (if-not (str/blank? c)
                      (let [v (parse-value sep s c)]
                        (if (not-empty? v)
                          (assoc res (keyword (:key s)) v)
                          res))
                      res)]
            (if (empty? cs)
              res
              (recur cs ss res)))))
      v)))


(defn parse-component [seps tp v]
  (if (:components tp)
    (let [cmps (split-by v (:component seps))]
      (loop [[c & cs] cmps
             [s & ss] (:components tp)
             res {}]
        (let [res (if-not (str/blank? c)
                    (let [v (parse-value seps s c)]
                      (if (not-empty? v)
                        (assoc res (keyword (:key s)) v)
                        res))
                    res)]
          (if (empty? cs)
            res
            (recur cs ss res)))))
    v))

(defn parse-field [seps {tpn :type c? :coll v :value :as f}]
  (let [tp (get types (keyword tpn))
        vv (if c?
             (->> (split-by v (:repetition seps))
                  (mapv #(parse-component seps tp %))
                  (filterv not-empty?))
             (parse-component seps tp v))]
    vv))


(defn parse-segment [seps seg]
  (let [fields (split-by seg (:field seps))
        [seg-name & fields] fields
        fields (if (= "MSH" seg-name)
                 (into ["|"] fields)
                 fields)

        seg-sch (get metadata (keyword seg-name))]

    [seg-name
     (loop [[s & ss] seg-sch
            field-idx 0
            acc {}]
       (let [f (nth fields field-idx nil)]
         (if (str/blank? f)
           (if (empty? ss)
             acc
             (recur ss (inc field-idx) acc))

           (let [v (parse-field seps (assoc (or s {}) :value f))
                 acc  (if (not-empty? v)
                        (let [k (:key s)]
                          (do
                            (println ">" (:key s))
                            (if (sequential? k)
                              (if-let [obj (and (map? (last k)) (last k))]
                                (do 
                                  (println "??"
                                           (butlast (mapv keyword k))
                                           obj
                                           v)
                                  (update-in acc (butlast (mapv keyword k))
                                             (fn [old]
                                               (into (or old [])
                                                     (if-let [nk (:$as obj)]
                                                       [(assoc (dissoc obj :$as) (keyword nk) v)]
                                                       (if (and (sequential? v) (not (string? v)))
                                                         (mapv #(merge obj %) v)
                                                         [(merge obj v)]))))))
                                (assoc-in acc (mapv keyword k) v))
                              (assoc acc (keyword k) v))))
                        acc)]
             (if (empty? ss)
               acc
               (recur ss (inc field-idx) acc))))))]))
