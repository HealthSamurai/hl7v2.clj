(ns hl7v2.bottomup)

(def rules
  {:insurance {:starts {:IN1 {}}
               :consumes {:IN2 {} :IN3 {}}}

   :message {:starts {:MSH {}}}
   :diagnosis {:starts {:DG1 {}}}
   :patient {:starts {:PID {}}
             :consumes {:PD1 {}}
             :groups {:visit {}
                      :allergy {:coll true}
                      :insurance {:coll true}
                      :diagnosis {:coll true}
                      :guarantor {:coll true}
                      :observation {:coll true}
                      :order {:coll true}}}

   :guarantor {:starts {:GT1 {}}}
   :allergy {:starts {:AL1 {}}}
   :comments  {:starts {:NTE {}}}
   :role {:starts {:ROL {}}}
   :visit {:starts {:PV1 {}}
           :consumes {:PV2 {}}}
   :timing {:starts {:TQ1 {}}
            :consumes {:TQ2 {:coll true}}}

   :specimen {:starts {:SPM {}}
              :consumes {:SAC {}}
              :groups {:observation {:coll true}}}

   :observation {:starts {:OBX {}}}
   :order {:starts {:ORC {} :OBR {}}
           :consumes {:OBR {}}
           :groups {:timing {:coll true}
                    :observation {:coll true}
                    :diagnosis {:coll true}
                    :specimen {:coll true}}}})

(defn mk-index [rules]
  (reduce (fn [acc [k {starts :starts :as group}]]
            (reduce (fn [acc [seg opts]]
                      (if (contains? acc seg)
                        (assert false (pr-str acc seg))
                        (assoc acc seg (assoc group :name k :opts opts))))
                    acc starts)
            ) {} rules))


(defn grouping [idx inp]
  (loop [parse-ctx nil
         cur-res {}
         [token & inp] inp
         res []]
    (if (nil? token)
      (if parse-ctx
        (conj res [parse-ctx cur-res])
        res)
      (if (and parse-ctx (contains? (:consumes parse-ctx) (first token)))
        (recur parse-ctx (merge cur-res (second token)) inp res)
        (let [res' (if parse-ctx (conj res [parse-ctx cur-res]) res)]
          (if-let [parse-ctx' (get idx (first token))]
            (recur parse-ctx' (second token) inp res')
            (assert false (str "No grup for " token))))))))

(defn structure [grps]
  (loop [gs (reverse grps)
         next-iter []]
    (if (empty? gs)
      next-iter
      (let [[{nm1 :name} v1 :as n1] (first gs)
            [{inc-grp :groups :as g2} v2 :as n2] (second gs)]
        (if-let [grp-opts (get inc-grp nm1)] 
          (recur (into (into next-iter [[g2 (update v2 nm1
                                                    (fn [old] (if old (if (sequential? old)
                                                                        (conj old v1) [old v1])
                                                                  (if (:coll grp-opts)
                                                                    [v1] v1))))]])
                       (rest (rest gs)))
                 [])
          (recur (rest gs) (conj next-iter n1)))))))

(defn *parse [ctx inp]
  (let [idx  (mk-index rules)
        grps (grouping idx inp)
        final-grps (structure grps)]
    (reduce (fn [acc [{nm :name} v]]
              (assoc acc nm v))
            {}
            final-grps)))

(defn parse [msg]
  (*parse {} msg))




