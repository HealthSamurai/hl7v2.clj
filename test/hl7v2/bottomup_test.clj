(ns hl7v2.bottomup-test
  (:require [hl7v2.bottomup :as sut]
            [matcho.core :as matcho]
            [hl7v2.core]
            [clojure.test :refer :all]))

(deftest bottom-up-parser-test

  (matcho/match
   (sut/parse [[:PID {:name 1}]])
   {:patient {:name 1}})


  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}]])
   {:patient {:name 1
              :insurance [{:in1 1}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}] [:IN2 {:in2 1}]])
   {:patient {:name 1
              :insurance [{:in1 1
                           :in2 1}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}]  [:IN1 {:in1 2}] [:IN2 {:in2 2}]])
   {:patient {:name 1
              :insurance [{:in1 1}
                          {:in1 2 :in2 2}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}] [:GT1 {:gt1 1}]])
   {:patient {:name 1
              :insurance [{:in1 1}]
              :guarantor [{:gt1 1}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}] [:GT1 {:gt1 1}] [:IN1 {:in1 2}] [:GT1 {:gt1 2}]])
   {:patient {:name 1
              :insurance [{:in1 1}
                          {:in1 2}]
              :guarantor [{:gt1 1}
                          {:gt1 2}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:PV1 {:pv1 1}]])
   {:patient {:name 1
              :visit {:pv1 1}}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}] [:IN1 {:in1 2}] [:GT1 {:gt1 1}] [:GT1 {:gt1 2}]])
   {:patient {:name 1
              :insurance [{:in1 1}
                          {:in1 2}]
              :guarantor [{:gt1 1}
                          {:gt1 2}]}})
  (matcho/match
   (sut/parse [[:PID {:name 1}] [:IN1 {:in1 1}] [:IN1 {:in1 2}] [:GT1 {:gt1 1}] [:GT1 {:gt1 2}] [:ORC {:orc 1}]])
   {:patient {:name 1
              :insurance [{:in1 1}
                          {:in1 2}]
              :guarantor [{:gt1 1}
                          {:gt1 2}]
              :order [{:orc 1}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:ORC {:orc 1}] [:OBR {:obr 1}]])
   {:patient {:name 1
              :order [{:orc 1 :obr 1}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}] [:ORC {:orc 1}] [:OBR {:obr 1}] [:OBX {:obx 1}] [:OBX {:obx 2}]])
   {:patient {:name 1
              :order [{:orc 1 :obr 1
                       :observation [{:obx 1}
                                     {:obx 2}]}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}]
               [:ORC {:orc 1}]
               [:OBR {:obr 1}]
               [:OBX {:obx 1}] [:OBX {:obx 2}]
               [:ORC {:orc 2}]
               [:OBX {:obx 21}]
               [:OBX {:obx 22}]])
   {:patient {:name 1
              :order [{:orc 1 :obr 1
                       :observation [{:obx 1}
                                     {:obx 2}]}
                      {:orc 2
                       :observation [{:obx 21}
                                     {:obx 22}]}]}})

  (matcho/match
   (sut/parse [[:PID {:name 1}]
               [:OBX {:obx 0}]
               [:ORC {:orc 1}]
               [:OBR {:obr 1}]
               [:OBX {:obx 1}] [:OBX {:obx 2}]])
   {:patient {:name 1
              :observation [{:obx 0}]
              :order [{:orc 1 :obr 1
                       :observation [{:obx 1}
                                     {:obx 2}]}]}})

  (def message [[:PID {:name 1}] [:IN1 {:in1 1}] [:IN2 {:in2 2}] [:GT1 {:gt1 1}] [:IN1 {:in 1}] [:ORC {:orc 1}] [:OBR {:obr 2}] [:OBX {:obx 3}] [:OBX {:obx 4}]])
  (sut/parse message)


  (def msg
   "MSH|^~\\&|app|PTG|Billing System|Billing System|20200203125130352||ORM^O01|4131405852|P|2.3|||AL|AL
PID|1||697909||Orange^Apple||19900214|F||^White|401 street^Apt 407^Xargo^CA^98980^USA^^^COUNTY||33333333^^^users@gmail.com|333333333|^English|||||||^No, Not Spanish/Hispanic/Latina||||||||N
PV1|1|O|PTG^^^PTG||||7218386992^John^Doe|1415710292^John^Doe|||||||||||1090949|||||||||||||||||||||||||202002031200||||||1090949
IN1|1|||Self Pay||||||||||||Orange^Apple|99|19900214|401 street^Apt 407^Xargo^CA^33770||||||||||||N||||||0||||||F||||C||617302
ORC|NW|LU344574|||||^^^^^R||20200203123404|155^Ogden^Sasha||456717292^Sarah^Smith
OBR|1|LU344574||39416^THINPREP PAP TEST^L|||20200203123340||||N|||||456717292^Sarah^Smith|||||||||||^^^^^R
DG1|1|I10|Z01.419^Encntr for gyn exam (general) (routine) w/o abn findings^I10|Encntr for gyn exam (general) (routine) w/o abn findings
DG1|2|I10|Z11.51^Encounter for screening for human papillomavirus (HPV)^I10|Encounter for screening for human papillomavirus (HPV)
OBX|1|ST|CLINICAL^Clinical Info||N/A
OBX|2|ST|CNTRCPTN^Contraception Use||Oral Contraception
OBX|3|ST|COLLTEC^Collection Technique||Spatula and brush
OBX|4|ST|CYTSRC^Cytology Source||Cervical, Endocervical
OBX|5|ST|EXAM^Today's Exam||Normal
OBX|6|ST|LMPDATE^LMP Date||20200120
OBX|7|ST|PAPHX^Previous History||Hx of Normal Paps
OBX|8|ST|PREGHX^Pregnancy Status||Not Pregnant
OBX|9|ST|PREVCYT^Previous Cytology||None N/A
OBX|10|ST|PREVTX^Previous Treatment||None
OBR|2|LU344574||39561^HPV HIGH RISK^L|||20200203125132||||N|||||456717292^Sarah^Smith|||||||||||^^^^^R
DG1|1|I10|Z01.419^Encntr for gyn exam (general) (routine) w/o abn findings^I10|Encntr for gyn exam (general) (routine) w/o abn findings
DG1|2|I10|Z11.51^Encounter for screening for human papillomavirus (HPV)^I10|Encounter for screening for human papillomavirus (HPV)
OBX|1|ST|CLINICAL^Clinical Info||N/A
OBX|2|ST|COLLTEC^Collection Technique||Spatula and brush
OBX|3|ST|CYTSRC^Cytology Source||Cervical, Endocervical ")

  (zprint.core/zprint
   (sut/parse
    (mapv (fn [[a b]] [(keyword a) b])
          (hl7v2.core/parse-only msg {:strict? false}))))

  )

