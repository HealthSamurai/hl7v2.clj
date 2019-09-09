(ns hl7v2.schema
  (:require [clojure.java.io :as io]
            [cheshire.core]
            [clj-yaml.core]
            [clojure.string :as str]))

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

(def *schema (atom nil))

(defn namify [s]
  (-> s
      (str/replace #"\(.*\)" "")
      (str/replace #"'s" "")
      (str/replace #"[^a-zA-Z0-9]+" "_")
      str/lower-case))

(namify "Mother's Maiden Name:")

(reset! *schema nil)

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

(defn gen-types []
  (let [fields (load-dir "fields")
        types (load-dir "dataTypes")
        types (->> types
                   (reduce
                    (fn [acc [k tp]]
                      (if (or (re-matches field-type-regex (name k))
                              (contains? primitives k))
                        acc
                        (assoc acc k
                               (if-let [comps (:components tp)]
                                 (update tp :components
                                         (fn [cmps]
                                           (mapv (fn [tp]
                                                   (let [s (get types (keyword (:dataType tp)))]
                                                     (cond->
                                                         {:type (:dataType s) :name (:dataType tp)}
                                                       (:longName s) (assoc :key (namify (:longName s)))
                                                       (:longName s) (assoc :desc (:longName s))
                                                       (= (:minOccurs tp) "1") (assoc :req true)
                                                       (= (:maxOccurs tp) "unbounded") (assoc :coll true))))
                                                 cmps)))
                                 tp)))
                      ) {}))]
    types))


(defn gen-segments []
  (let [fields (load-dir "fields")]
    (->> (load-dir "segments")
         (reduce (fn [acc [k {flds :fields}]]
                   (assoc acc k
                          (->> flds
                               (mapv (fn [{nm :field :as f}]
                                       (let [s (get fields (keyword nm))]
                                         (cond-> {:name (:field f)
                                                  :type (:dataType s)
                                                  :key (namify (:longName s))
                                                  :desc (:longName s)}
                                           (not (str/blank? (:hl7Table s))) (assoc :table (:hl7Table s))
                                           (= (:minOccurs f) "1") (assoc :req true)
                                           (= (:maxOccurs f) "unbounded") (assoc :coll true))))))
                          )
                   ) {}))))


(defn calculate-stm [el els]
  (loop [[e & es] els
         res (if (:coll el)
               (if-let [s (:seg el)]
                 {:seg {(keyword s) (:pth el)} :grp {}}
                 {:grp {(keyword (:grp el)) (:pth el)} :seg {}}))]
    (when e
      (let [s (keyword (:seg e))
            g (keyword (:grp e))
            res (cond-> res
                  s (assoc-in [:seg s] (:pth e))
                  g (assoc-in [:grp g] (:pth e)))]
        (if (empty? es)
          res
          (if (:req e)
            res
            (recur es res)))))))

(defn calculate-enter [el]
  (loop [[e & es] (:els el)
         res {}]
    (when e
      (let [s (keyword (:seg e))
            g (keyword (:grp e))
            res (cond-> res
                  s (assoc-in [:seg s] (:pth e))
                  g (assoc-in [:grp g] (:pth e)))]
        (if (empty? es)
          res
          (if (:req e)
            res
            (recur es res)))))))

(defn state-machine [els]
  (loop [[el & els] els
         res []]
    (let [res (conj res (cond-> (assoc el :stm (-> (calculate-stm el els)))
                          (:grp el) (assoc :enter (calculate-enter el))
                          (:els el) (assoc :els (state-machine (:els el)))))]
      (if (empty? els)
        res
        (recur els res)))))

(defn collect-groups [acc els]
  (reduce (fn [acc el]
            (let [acc (if (:els el)
                        (collect-groups acc (:els el))
                        acc)]
              (if-let [g (:grp el)]
                (assoc acc (keyword g) (:enter el))
                acc)))
          acc els))

(defn res-grp [grps]
  (reduce (fn [acc [k {grp :grp :as g}]]
            (if-not grp
              (assoc acc k g)
              (update acc k
                      #(merge-with merge %
                                   (reduce (fn [stm [gn loc]]
                                             (let [rg (get grps (keyword gn))]
                                               (merge-with merge stm rg)))
                                           {} grp))))
            ) {} grps))

(defn rep-groups [grps els]
  (->> els
       (mapv
        (fn [{{gstm :grp segs :seg :as stm} :stm :as el}]
          (cond-> (dissoc el :enter :idx)  
            (or gstm segs) (assoc :stm (reduce (fn [acc [gn loc]]
                                       (if-let [g (get grps (keyword gn))]
                                         (merge acc (:seg g))
                                         acc)) (or segs {}) gstm))
            (:els el) (assoc :els (rep-groups grps (:els el)))
            )))))

(defn resolve-stm [els]
  (let [grps (collect-groups {} els)
        grps (loop [prev grps]
               (let [new (res-grp prev)]
                 (if (= prev new)
                   prev
                   (recur new))))]
    (rep-groups grps els)))

(defn gen-msg [msg]
  (reduce
   (fn [acc [k' v]]
     (assoc acc k'
            (update v :elements
                    (fn [els]
                      (->> els
                           (map-indexed (fn [i el]
                                          (cond-> {:idx i}
                                            (:segment el) (assoc :seg (:segment el))
                                            (:group el) (assoc :grp (:group el))
                                            (= (:minOccurs el) "1") (assoc :req true)
                                            (= (:maxOccurs el) "unbounded") (assoc :coll true)))))))))
   {} msg))


(defn resolve-groups [pth grps msg]
  (->> (:elements msg)
       (mapv (fn [{g :grp :as el}]
               (-> (if g
                     (let [k (keyword g)
                           gr (get grps k)
                           resolved-gr (resolve-groups (conj pth (:idx el)) grps gr)]
                       (assoc el :els resolved-gr))
                     el)
                   (assoc :pth (conj pth (:idx el))))))))

(defn gen-groups [msgs]
  (->> msgs
       (reduce
        (fn [acc [k grps]]
          (let [msg (get grps k)]
            (assoc acc k (-> (resolve-groups [] grps msg)
                             (state-machine)
                             (resolve-stm)))))
        {} )))

(defn gen-messages []
  (->> (load-dir "hl7messages")
       (reduce (fn [acc [k parts]]
                 (assoc acc k (-> (gen-msg parts)))) {})
       (gen-groups)))

;; (spit "resources/types.yaml"
;;       (clj-yaml.core/generate-string (gen-types)))

;; (spit "resources/segments.yaml"
;;       (clj-yaml.core/generate-string (gen-segments)))

(def oru (:ORU_R01 (gen-messages)))


(spit "resources/messages.yaml"
      (clj-yaml.core/generate-string
       (:ORU_R01 (gen-messages))))


(defn schema []
  (if-let [s  @*schema]
    s
    (reset! *schema
            (let [fields (load-dir "fields")
                  types (load-dir "dataTypes")
                  sch {:types (->> types
                                   (reduce
                                    (fn [acc [k tp]]
                                      (if (re-matches #".*\.\d+$" (name k))
                                        acc
                                        (assoc acc k
                                               (if-let [comps (:components tp)]
                                                 (update tp :components
                                                         (fn [cmps]
                                                           (mapv (fn [tp]
                                                                   (let [s (get types (keyword (:dataType tp)))]
                                                                     (cond->
                                                                         {:type (:dataType s) :nm (:dataType tp)}
                                                                       (:longName s) (assoc :name (namify (:longName s)))
                                                                       (:longName s) (assoc :desc (:longName s))
                                                                       (= (:minOccurs tp) "1") (assoc :req true)
                                                                       (= (:maxOccurs tp) "unbounded") (assoc :coll true))))
                                                                 cmps)))
                                                 tp)))
                                      ) {}))
                       :messages (load-dir "messages")
                       :segments (->> (load-dir "segments")
                                      (reduce (fn [acc [k {flds :fields}]]
                                                (assoc acc k
                                                       (->> flds
                                                            (mapv (fn [{nm :field :as f}]
                                                                    (let [s (get fields (keyword nm))]
                                                                      (cond-> {:filed (:field f)
                                                                               :type (:dataType s)
                                                                               :name (namify (:longName s))}
                                                                        (not (str/blank? (:hl7Table s))) (assoc :table (:hl7Table s))
                                                                        (= (:minOccurs f) "1") (assoc :req true)
                                                                        (= (:maxOccurs f) "unbounded") (assoc :coll true))))))
                                                       )
                                                ) {}))
                       :structure (:index (load-dir "structure"))}]

              sch))))

;; (spit "/tmp/hl.yaml"
;;       (clj-yaml.core/generate-string 
;;        (schema)))

(comment 
  )
