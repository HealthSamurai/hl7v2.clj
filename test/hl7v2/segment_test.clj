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
      {:phone "333333333", :system "phone", :use "work"}]}])

  (matcho/match
   (sut/parse-segment
    separators
    "IN1|1|11111111^AMB-ANTHEM BLUE ACCESS-BRANSO-BBA^^^AMB-ANTHEM BLUE ACCESS-BRANSO-BBA|222222|Missouri Blue Cross Blue Shield|PO BOX 33333^ANYWHERE^ATLANTA^GA^44444^CD:55555^B||1111111^WPN^CD:6666|77777||88888||20190101000000|||CD:999|john^doe^K^^^^CURRENT|CD:211111|2222222|2333333 STATE HIGHWAY T^^BTANTOT^MO^244444^CD:25555^M~2666 STATE HIGHWAY T^^BTANTOT^MO^27777^CD:28888^H~USER@COM.COM^^^^^^CD:2888|||1|||||||||||||CD:2999||||||||F|Anywhere^^Anywhere^MO^311111^CD:22133^B|||||A3222222")
   ["IN1"
    {:_seqno "1",
     :beneficiary
     {:address [{:city "BTANTOT",
                 :country "CD:25555",
                 :postal_code "244444",
                 :state "MO",
                 :street {:text "2333333 STATE HIGHWAY T"},
                 :type "M"}
                {:city "BTANTOT",
                 :country "CD:28888",
                 :postal_code "27777",
                 :state "MO",
                 :street {:text "2666 STATE HIGHWAY T"},
                 :type "H"}
                {:street {:text "USER@COM.COM"}, :type "CD:2888"}],
      :birth_date {:time "2222222"},
      :employer {:address [{:city "Anywhere",
                            :country "CD:22133",
                            :postal_code "311111",
                            :state "MO",
                            :street {:text "Anywhere"},
                            :type "B"}]},
      :gender "F",
      :name [{:family {:surname "john"},
              :given "doe",
              :initials "K",
              :type "CURRENT"}],
      :relationship {:code "CD:211111"},
      :subscriber_id [{:value "A3222222"}]},
     :benifits {:priority "1"},
     :company_plan "CD:2999",
     :group {:employee {:identifier [{:value "88888"}]}, :id "77777"},
     :payor {:address [{:city "ATLANTA",
                        :country "CD:55555",
                        :postal_code "44444",
                        :state "GA",
                        :street {:text "PO BOX 33333"},
                        :text "ANYWHERE",
                        :type "B"}],
             :contact {:telecom [{:phone "1111111", :type "CD:6666", :use "WPN"}]},
             :identifier [{:value "222222"}],
             :name [{:name "Missouri Blue Cross Blue Shield"}]},
     :period {:start "20190101000000"},
     :plan {:identifier {:alternate_display "AMB-ANTHEM BLUE ACCESS-BRANSO-BBA",
                         :code "11111111",
                         :display "AMB-ANTHEM BLUE ACCESS-BRANSO-BBA"},
            :type "CD:999"}}]
   )
  

  (matcho/match
   (sut/parse-segment
    separators
    "IN2|empid:11111|ssn:222|||||||||||||||||||||||payor:3333||||||||||||||||||||||||||||||||||||mem:3333||(555)5555-4384^PRN^CD:193972~(555)555-4384^CP^CD:78193972~(555)555-4384^CP^CD:78193972|(111)111-1111^WPN||||||||CD:24246")
   ["IN2"
    {:beneficiary {:employee {:identifier [{:value "empid:11111"}]},
                   :identifier [{:system "ssn", :value "ssn:222"}
                                {:system "member-number" :value "mem:3333"}]
                   :telecom
                   [{:phone "(555)5555-4384",
                     :system "phone",
                     :type "CD:193972",
                     :use "PRN"}
                    {:phone "(555)555-4384",
                     :system "phone",
                     :type "CD:78193972",
                     :use "CP"}
                    {:phone "(555)555-4384",
                     :system "phone",
                     :type "CD:78193972",
                     :use "CP"}
                    {:phone "(111)111-1111", :system "phone", :use "WPN"}]},
     :cms {:relationship {:code "CD:24246"}},
     :payor {:id [{:value "payor:3333"}]}}])

  )

