(ns hl7v2.schema.parsec-test
  (:require [hl7v2.schema.parsec :as sut]
            [clojure.test :refer :all]))

(defmacro parsed-as [g a b]
  `(is (= (sut/parse ~g ~a) ~b)))

(deftest test-parser 

  (parsed-as
   {:msg ["A" "B*" "C"]}
   [:A :B :B :C]
   {:A 0, :B [1 2]})
  
  

  (def grm1 {:msg     ["M" "pt*"]
             :pt      ["A" "visit*" "F"]
             :visit   ["B" "details?"]
             :details ["D"]})


  (parsed-as
   grm1
   [:M :A :B :B :D :B :F :A :B :B :D :B :F]
   {:M 0,
    :pt [{:A 1, :visit [{:B 2} {:B 3, :details {:D 4}} {:B 5}], :F 6}
         {:A 7, :visit [{:B 8} {:B 9, :details {:D 10}} {:B 11}], :F 12}]})

  )
