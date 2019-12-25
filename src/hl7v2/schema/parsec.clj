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

;; (def foo (atom 0))

(defn do-parse [grammar get-value rule inp]
  ;; (when (< @foo 20)
  ;;   (println ">>>> rule" (get grammar rule) inp)
  ;;   (swap! foo inc))

  (loop [[stm & stms :as sstms] (get grammar rule)
         inp inp
         out {}
         repeat true]

    (if (nil? stm)
      [inp out]
      (if-let [c (cur inp)]
        (let [tp (if (seg? stm) :seg :grp)
              [nm q] (name-quant stm)]
          (cond
            (= tp :seg) (if (= nm c)
                          (if (contains? #{:+ :*} q)
                            (recur sstms (inext inp)
                                   (update out c (fn [x] (conj (or x []) (get-value (:pos inp)))))
                                   true)

                            (recur stms (inext inp)
                                   (assoc out c (get-value (:pos inp)))
                                   false))
                          (cond
                            (or (= q :*) (and repeat (= q :+)))
                            (recur stms inp out false)

                            (= q :?)
                            (recur stms inp out false)

                            :else
                            [inp [:error (str "Rule " rule " [" (str/join " " (get grammar rule)) "] at " stm  " expected  [" (name nm) "] got [" (name c) "] segment position " (:pos inp))]]))

            (= tp :grp) (let [[inp' res] (do-parse grammar get-value nm inp)]
                          (if-not (= :error (first res))
                            (if (and (contains? #{:+ :*} q)
                                     (not= (:pos inp) (:pos inp')))
                              (recur sstms inp' (update out nm (fn [x] (conj (or x []) res))) true)
                              (recur stms inp' (assoc out nm res) false))
                            (cond
                              (or (= q :*) (and repeat (= q :+)))
                              (recur stms inp out false)

                              (= q :?)
                              (recur stms inp out false)

                              :else
                              [inp res])))))

        (if (or (str/index-of stm "*") (str/index-of stm "?"))
          [inp out]

          (if repeat
            [inp out]
            [inp [:error (str "Rule " rule " [" (str/join " " (get grammar rule)) "] at " stm  " expected " stm " at segment position " (:pos inp))]]))))))

(defn parse [grammar msg get-value opts]
  (let [[inp res] (do-parse grammar (or get-value identity) :msg {:msg msg :pos 0})]
    (if (= :error (first res))
      res
      (if (= (:pos inp) (count (:msg inp)))
        res
        (if (get opts :strict? true)
          [:error (str "Extra input [" (name (cur inp)) "] pos: " (:pos inp))]
          res)))))

