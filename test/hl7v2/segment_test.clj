(ns hl7v2.segment-test
  (:require [hl7v2.segment :as sut]
            [clojure.test :refer :all]
            [matcho.core :as matcho]))

(def separators
  {:segment #"(\r\n|\r|\n)"
   :field "|"
   :component "^"
   :subcomponet "&"
   :repetition "~"
   :escape "\\"})

(deftest test-segment

  (matcho/match
   (sut/parse-segment
    separators
    "PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N")

   ["PID"
    {:_seqno "1",
     :account {:identifier {:authority "MS4001",
                            :facility "001",
                            :system "AN",
                            :value "111155555550"}},
     :address [{:city "ALBUKERKA",
                :country "USA",
                :postal_code "98765",
                :state "CA",
                :street {:text "STRAWBERRY AVE"},
                :text "FOUR OAKS LODGE"}],
     :birth {:place "OKLAHOMA"},
     :birth_date {:time "19440823"},
     :death {:indicator "N"},
     :gender "F",
     :identifier [{:authority "MS4", :system "PN", :use "official", :value "010107111"}
                  {:authority "MS4", :facility "001", :system "MR", :value "1609220"}
                  {:authority "MS4",:facility "001",:system "MR",:use "secondary",:value "1609220"}
                  {:system "ssn", :value "123-22-1111"}],
     :language {:code "ENG"},
     :marital_status {:code "W"},
     :name [{:family {:surname "BARRETT"},
             :given "JEAN",
             :initials "SANDY",
             :use "official"}],
     :race [{:code "C"}],
     :religion {:code "CHR"},
     :telecom [{:phone "(111)222-3333", :system "phone", :use "home"}]}])


  (matcho/match
   (sut/parse-segment
    separators
    "PID|1||697909||Orange^Apple||19900214|F||^White|401 street^Apt 407^Xargo^CA^98980^USA^^^COUNTY||33333333^^^users@gmail.com|333333333|^English|||||||^No, Not Spanish/Hispanic/Latina||||||||N")

   ["PID"
    {:_seqno "1",
     :address [{:city "Xargo",
                :country "USA",
                :county "COUNTY",
                :postal_code "98980",
                :state "CA",
                :street {:text "401 street"},
                :text "Apt 407"}],
     :birth_date {:time "19900214"},
     :death {:indicator "N"},
     :ethnicity [{:display "No, Not Spanish/Hispanic/Latina"}],
     :gender "F",
     :identifier [{:value "697909"}],
     :language {:display "English"},
     :name [{:family {:surname "Orange"}, :given "Apple", :use "official"}],
     :race [{:display "White"}],
     :telecom
     [{:email "users@gmail.com", :phone "33333333", :system "phone", :use "home"}
      {:phone "333333333", :system "phone", :use "work"}]}]
   )
  )

