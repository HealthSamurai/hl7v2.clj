(ns hl7v2.schema.types-test
  (:require [hl7v2.schema.types :as sut]
            [clojure.test :refer :all]))

(deftest format-datetime
  (is (= (sut/format-datetime "20120529130917-04:00") "2012-05-29T13:09:17-04:00"))

  (is (= (sut/format-datetime "20050110045253") "2005-01-10T04:52:53"))

  (is (= (sut/format-datetime "19450409") "1945-04-09"))
  )
