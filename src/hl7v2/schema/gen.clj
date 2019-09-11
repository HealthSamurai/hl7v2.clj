(ns hl7v2.schema.gen
  (:require [clojure.java.io :as io]
            [cheshire.core]
            [clj-yaml.core]
            [clojure.string :as str]))


(defn reduce-map [f m]
  (reduce (fn [acc [k v]]
            (if-let [v' (f k v)]
              (assoc acc k v')
              acc)) {} m))

(defn load-dir [dir]
  (->> (file-seq (clojure.java.io/file (io/file (.getPath (io/resource dir)))))
       (reduce
        (fn [acc f]
          (let [nm (.getName f)]
            ;; (println nm)
            (if (str/ends-with? nm ".json")
              (let [k (first (str/split nm #"\.json$"))]
                (assoc acc (keyword k) (cheshire.core/parse-string (slurp (.getPath f)) keyword)))
              acc)))
        {})))

(defn namify [s]
  (-> s
      (str/replace #"\(.*\)" "")
      (str/replace #"'s" "")
      (str/replace #"[^a-zA-Z0-9]+" "_")
      str/lower-case))

(namify "Mother's Maiden Name:")

(defn normalize-el [obj el]
  (cond-> obj
      (:segment el) (assoc :seg (:segment el))
      (:group el) (assoc :grp (:group el))
      (= (:minOccurs el) "1") (assoc :req true)
      (= (:maxOccurs el) "unbounded") (assoc :coll true)))

(def field-type-regex  #".*\.\d+$")

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
   :ST true
   :TM true
   :TX true
   :escapeType true
   :varies true})

(defn skip-type? [k]
  (or (re-matches field-type-regex (name k))
      (contains? primitives k)))

(defn gen-types []
  (let [fields (load-dir "fields")
        types (load-dir "dataTypes")]
    (->> types
         (reduce-map (fn [k tp]
                       (when-not  (skip-type? k)
                         (if-let [comps (:components tp)]
                           (update tp :components
                                   (fn [cmps]
                                     (mapv (fn [tp]
                                             (let [s (get types (keyword (:dataType tp)))]
                                               (cond-> (normalize-el {:type (:dataType s) :name (:dataType tp)} tp)
                                                 (:longName s) (assoc :key (namify (:longName s)))
                                                 (:longName s) (assoc :desc (:longName s)))))
                                           cmps)))
                           tp)))))))

(defn gen-segment [fields {flds :fields}]
  (->> flds
       (mapv (fn [{nm :field :as f}]
               (let [s (get fields (keyword nm))]
                 (cond-> {:name (:field f)
                          :type (:dataType s)
                          :key (namify (:longName s))
                          :desc (:longName s)}
                   (not (str/blank? (:hl7Table s))) (assoc :table (:hl7Table s))
                   (= (:minOccurs f) "1") (assoc :req true)
                   (= (:maxOccurs f) "unbounded") (assoc :coll true)))))))

(defn gen-segments []
  (let [fields (load-dir "fields")]
    (->> (load-dir "segments")
         (reduce-map
          (fn [k seg]
            (gen-segment fields seg))))))

#_(->> msg
       (reduce-map
        (fn [k' v]
          {:els (->> (:elements v)
                     (map-indexed (fn [i el] (normalize-el {:idx i} el))))})))

(defn gen-messages []
  (-> 
   (->> (load-dir "hl7messages")
        (reduce-map
         (fn [k msg]
           (reduce
            (fn [acc [k' {els :elements}]]
              (let [stms (->> els
                              (mapv (fn [e]
                                      (let [el (normalize-el {} e)
                                            nm (if (:grp el)
                                                 (str/lower-case (:grp el))
                                                 (:seg el))
                                            q (cond (and (:coll el) (:req el)) "+"
                                                    (:coll el) "*"
                                                    (:req el)  ""
                                                    :else "?")]
                                        (str nm q)))))]
                (assoc acc (if (= k k') :msg (keyword (str/lower-case (name k')))) stms)))
            {} msg))))
   (assoc :idx (cheshire.core/parse-string (slurp (.getPath (io/resource "structure/index.json"))) keyword))))

(defn generate []
  (spit "resources/types.yaml" (clj-yaml.core/generate-string (gen-types)))
  (spit "resources/segments.yaml" (clj-yaml.core/generate-string (gen-segments)))
  (spit "resources/messages.yaml" (clj-yaml.core/generate-string (gen-messages)))


  )

(comment

  (cheshire.core/parse-string (slurp "schema/structure/index.json") keyword)

  (generate)

  )





