(ns hl7v2.schema.parsec
  (:require [clojure.string :as str]))


(defn cur [inp]
  (get (:msg inp) (:pos inp)))

(defn inext [inp]
  (update inp :pos inc))

(defn seg? [x]
  (let [fl (subs x 0 1)]
    (= fl (str/upper-case fl))))

(defn name-quant [x]
  (if-let [[_ nm q] (re-find #"(.*)(\?|\*|\+)$" x)]
    [(keyword nm) (keyword q)]
    [(keyword x) nil]))

(name-quant "xxxssss?")

(name-quant "xxxssss")

(name-quant "xxxssss*")

(name-quant "xxxssss+")

(defn do-parse [grammar rule inp]
  (loop [[stm & stms :as sstms] (get grammar rule)
         inp inp
         out {}]
    (println rule inp stm)
    (if (nil? stm)
      [inp out]
      (if-let [c (cur inp)]
        (let [tp (if (seg? stm) :seg :grp)
              [nm q] (name-quant stm)]
          (cond
            (= tp :seg) (if (= nm c)
                          (if (contains? #{:+ :*} q )
                            (recur sstms (inext inp) (update out c (fn [x] (conj (or x []) (:pos inp)))))
                            (recur stms (inext inp)  (assoc out c (:pos inp))))
                          (if (contains? #{:? :*} q)
                            (recur sstms (inext inp) out)
                            [inp [:error (pr-str "Expected " nm "got " c)]]))

            (= tp :grp) (let [[inp' res] (do-parse grammar nm inp)]
                          (if-not (= :error (first res))
                            (if (contains? #{:+ :*} q)
                              (recur sstms inp' (update out nm (fn [x] (conj (or x []) res))))
                              (recur stms inp' (assoc out nm res)))
                            (if (contains? #{:? :*} q)
                              (recur stms inp out)
                              [inp res])))))
        [inp out]))))

(defn parse [grammar msg]
  (second (do-parse grammar :msg {:msg msg :pos 0})))

