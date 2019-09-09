(ns hl7v2.schema.parser)

(defn reduce-map [f m]
  (reduce (fn [acc [k v]]
            (if-let [v' (f k v)]
              (assoc acc k v')
              acc)) {} m))

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
                  s (assoc-in [:seg s] (:idx e))
                  g (assoc-in [:grp g] (:idx e)))]
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

(defn resolve-groups [pth grps msg]
  (->> (:els msg)
       (mapv (fn [{g :grp :as el}]
               (-> (if g
                     (let [k (keyword g)
                           gr (get grps k)
                           resolved-gr (resolve-groups (conj pth (:idx el) :els) grps gr)]
                       (assoc el :els resolved-gr))
                     el)
                   (assoc :pth (conj pth (:idx el))))))))


(defn gen-groups [msgs]
  msgs
  (->> msgs
       (reduce-map
        (fn [k grps]
          (->> grps
               (reduce-map
                (fn [k' {els :els :as grp}]
                  (assoc grp :enter (calculate-enter grp))))
               (resolve-enter))))))
