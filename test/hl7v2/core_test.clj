(ns hl7v2.core-test
  (:require [hl7v2.core :as sut]
            [clj-yaml.core]
            [hl7v2.schema.core :as schema]
            [matcho.core :refer [match]]
            [zprint.core :as zp]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all])
  (:import [java.io File]))

(def extensions
  [[:ADT_A01 :ZBC [[:name "ZBC.1" :type "ST" :key "zbc_one"]
                   [:name "ZBC.2" :type "ST" :key "zbc_two"]
                   [:name "ZBC.3" :type "ST" :key "zbc_three"]]]
   [:ADT_A01 :ZG1 [[:name "ZG1.1" :type "ST" :key "zg1_one"]
                   [:name "ZG1.2" :type "ST" :key "zg1_two"]
                   [:name "ZG1.3" :type "ST" :key "zg1_three"]
                   [:name "ZG1.4" :type "ST" :key "zg1_four"]] {:after "GT1" :quant "*"}]
   [:ADT_A01 :ZPD [[:name "ZPD.1" :type "ST" :key "zpd1"]
                   [:name "ZPD.2" :type "ST" :key "zpd2"]
                   [:name "ZPD.3" :type "ST" :key "zpd3"]
                   [:name "ZPD.4" :type "ST" :key "zpd4"]
                   [:name "ZPD.5" :type "ST" :key "zpd5"]
                   [:name "ZPD.6" :type "ST" :key "zpd6"]
                   [:name "ZPD.7" :type "ST" :key "zpd7"]
                   [:name "ZPD.8" :type "ST" :key "zpd8"]
                   [:name "ZPD.9" :type "ST" :key "zpd9"]
                   [:name "ZPD.10" :type "ST" :key "zpd10"]
                   [:name "ZPD.11" :type "ST" :key "zpd11"]
                   [:name "ZPD.12" :type "ST" :key "zpd12"]] {:after "PID"}]])

(def msg
  "MSH|^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||
EVN|A01|20151010045502|||||
PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N
PV1|1|I|PREOP^101^1^1^^^S|3|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|||01||||1|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|2|40007716^^^AccMgr^VN|4|||||||||||||||||||1||G|||20050110045253||||||
GT1|1|010107127^^^MS4^PN^|BARRETT^JEAN^S^^|BARRETT^LAWRENCE^E^^|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^|(818)111-3361||19301013|F||A|354-22-1840||||RETIRED|^^^^00000^|||||||20130711|||||0000007496|W||||||||Y|||CHR||||||||RETIRED||||||C
IN1|1|0423|MEDICARE IP|^^^^     |||||||19951001|||MCR|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||1||||||||||||||354221840A|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||354221840A||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C
IN1|2|0423|2304|AETNA PPO|PO BOX 14079^PO BOX 14079^LEXINGTON^KY^40512|||081140101400020|RETIRED|||20130101|||COM|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||2||||||||||||||811001556|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C
")

(deftest test-parsing
  (reset! schema/*schema nil)

  (def schema (schema/schema))
  (def seps {:field \|,
             :component \^,
             :subcomponet \&,
             :repetition \~,
             :escape \\})

  (def ctx 
    {:schema schema
     :separators seps})

  (is (= (dissoc (sut/separators msg) :segment) seps))

  (testing "segments"
    (match 
     (sut/parse-segment
      ctx "MSH|^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||" {})
     ["MSH"
      {:type {:code "ADT", :event "A01"},
       :id "599102",
       :encoding "^~\\&",
       :seqnum "foo",
       :version {:id "2.3"},
       :proc_id {:proc_id "P"},
       :datetime {:time "20151015200643"},
       :facility {:ns "1"},
       :app {:ns "AccMgr"}
       :separator "|"
       }])

    (match
     (sut/parse-segment
      ctx "PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N" {})
     ["PID"
      {:religion {:code "CHR"},
       :patient_id  {:id "010107111",
                    :authority {:ns "MS4"},
                    :type "PN"},
       :alternate_id [{:id "1609220",
                       :authority {:ns "MS4"},
                       :type "MR",
                       :facility {:ns "001"}}],
       :account_number {:id "111155555550",
                        :authority {:ns "MS4001"},
                        :type "AN",
                        :facility {:ns "001"}},
       :race [{:code "C"}],
       :gender "F",
       :birth_date {:time "19440823"},
       :primary_language  {:code "ENG"},
       :home_phone [{:phone "(111)222-3333"}],
       :ssn_number "123-22-1111",
       :birth_place "OKLAHOMA",
       :address [{:street {:text "STRAWBERRY AVE"}
                  :text "FOUR OAKS LODGE",
                  :city "ALBUKERKA",
                  :state "CA",
                  :postal_code "98765",
                  :country "USA"}],
       :set_id "1",
       :name [{:family {:surname "BARRETT"},
               :given "JEAN",
               :initials "SANDY"}],
       :identifiers [{:id "1609220",
                      :authority {:ns "MS4"},
                      :type "MR",
                      :facility {:ns "001"}}],
       :marital_status {:code "W"},
       :death_indicator "N"}])

    (match
     (sut/parse-segment
      ctx "PID|1|312626^^^^^Main Lab&05D0557149&CLIA|0362855^^^^^Main Lab&05D0557149&CLIA|^^^^^Main Lab&05D0557149&CLIA|LOPEZ^ADALBERTO||19450409|M|||8753 APPERSON ST^^SUNLAND^CA^91040||(818)429-5631|||||000016715153|572458313" {})

     ["PID" {:identifiers [{:id "0362855",
                            :facility {:ns "Main Lab", :uid "05D0557149", :type "CLIA"}}]}]
     )

    (match
     (sut/parse-segment
      ctx "IN1|1|303401^PRIV HLTH CARE SYS-BCBS OF N CAROLINA|3034|BCBS OF NORTH CAROLINA|CLMS PROCESSING CONTRACTOR PO BOX 9518^^DURHAM^NC^32145-9518^||(845)543-3876^^^^^845^5433876|1233||||20160726||||UPGRADETEST^CPAP^^|Self|19490512|876 MAIN^^BANANA VALLEY^WA^98038^US^^^KING|||1**1|||NO||||20170726102055|BARLLH1^BARLOW^LOUIS^H.^|||||4694998|F8086412450||||||Full|M|1258 ROSE AVE SW^^RENTON^WA^98057^US|Verified Pat||BOTH||" {})

     ["IN1" {:beneficiary_address [{:city "BANANA VALLEY", :country "US", :county "KING", :postal_code "98038", :state "WA", :street {:text "876 MAIN"}}], :beneficiary_birthDate {:time "19490512"}, :beneficiary_gender "M",
             :beneficiary_name [{:family {:surname "UPGRADETEST"}, :given "CPAP"}], :benifits_coordination_priority "1**1", :class_type "BOTH", :contract_identifier "F8086412450", :eligibility_flag "NO",
             :employer_address [{:city "RENTON", :country "US", :postal_code "98057", :state "WA", :street {:text "1258 ROSE AVE SW"}}], :employment_status {:code "Full"}, :group_number "1233",
             :identifier_type {:code "303401", :display "PRIV HLTH CARE SYS-BCBS OF N CAROLINA"},
             :payor_organization_address [{:city "DURHAM", :postal_code "32145-9518", :state "NC", :street {:text "CLMS PROCESSING CONTRACTOR PO BOX 9518"}}],
             :payor_organization_contact_telecom [{:area_city "845", :local_number "5433876", :phone "(845)543-3876"}], :payor_organization_identifier_value [{:id "3034"}],
             :payor_organization_name [{:name "BCBS OF NORTH CAROLINA"}], :period_start "20160726", :relationship_to_patient {:code "Self"}, :set_id "1",
             :verification_by [{:family {:surname "BARLOW"}, :given "LOUIS", :id "BARLLH1", :initials "H."}], :verification_datetime {:time "20170726102055"}, :verification_status "Verified Pat"}]
     )
    ))

(defn expectation-file [^File f]
  (let [name (.getName f)
        parent (-> f .getParentFile .getAbsoluteFile .getPath)
        edn-path (string/replace parent #"messages" "edns")
        expectation (str edn-path "/" name ".edn")]
    (io/make-parents expectation)
    expectation))

(defn match-file [fname content]
  (match (-> fname slurp edn/read-string)
         content))

(defn read-files []
  (->> "messages" io/resource io/file file-seq (filter #(.isFile %))))

(defmacro foreach-hl7 [[input expected] & body]
  `(doseq [^File input-file# (read-files)]
     (let [~input (slurp input-file#)
           ~expected (expectation-file input-file#)]
       ~@body)))

(defn rewrite-hl7-edn [fname]
  (let [f (-> (str "messages/" fname) io/resource io/file)]
    (spit (expectation-file f)
          (zp/zprint-str (sut/parse (slurp f) {:extensions extensions})))))

(deftest test-parse-examples
  (foreach-hl7 [input expected]
               (testing (str "with " expected)
                 (match-file expected (sut/parse input {:extensions extensions :strict? false})))))

(def silly-msg-1 "MSH|^~\\&|VITAEHR|ORCA||AS_AFTER_CHECK_IN|20170807134552|D454345|ADT^A04|541642|T|2.3|||||||||||
  EVN|A04
  PID|1")

(def silly-msg-2 "MSH|^~\\&|VITAEHR|ORCA||AS_AFTER_CHECK_IN|20170807134552|D454345|ADT^A04|541642|T|2.3|||||||||||
  EVN|A04|20170807152032||AS_AFTER_CHECK_IN|^DYER^ADRIANNA^R^^^^^JHC^^^^^JHCCC||")

(deftest silly-message-test
  (is (= [:error "(\"Field EVN.2 is required\" \"Field PID.3 is required\" \"Field PID.5 is required\")"]
         (sut/parse silly-msg-1)))

  (is (= [:error "Rule :msg [MSH SFT* EVN PID PD1? CON* ROL* NK1* PV1 PV2? ROL* DB1* OBX* AL1* DG1* DRG? procedure* GT1* insurance* ACC? UB1? UB2? PDA?] at PID expected PID at segment position 2"]
         (sut/parse silly-msg-2))))





































(comment
  (spit "/tmp/1.yaml" (clj-yaml.core/generate-string (sut/parse-segment
                                                      ctx "IN1|1|303401^PRIV HLTH CARE SYS-BCBS OF N CAROLINA|3034|BCBS OF NORTH CAROLINA|CLMS PROCESSING CONTRACTOR PO BOX 9518^^DURHAM^NC^32145-9518^||(845)543-3876^^^^^845^5433876|1233||||20160726||||UPGRADETEST^CPAP^^|Self|19490512|876 MAIN^^BANANA VALLEY^WA^98038^US^^^KING|||1**1|||NO||||20170726102055|BARLLH1^BARLOW^LOUIS^H.^|||||4694998|F8086412450||||||Full|M|1258 ROSE AVE SW^^RENTON^WA^98057^US|Verified Pat||BOTH||")))

  (spit "test/results/adt.yaml" (clj-yaml.core/generate-string (sut/parse msg {})))


  (def oru "MSH|^~\\&|LAb|Lab|Lab|Lab|20100222024516||ORU^R30|87226A1476977481|P|2.4|2010022217399||AL|NE
PID|1|312626^^^^^Main Lab&05D0557149&CLIA|0362855^^^^^Main Lab&05D0557149&CLIA|^^^^^Main Lab&05D0557149&CLIA|LOPEZ^ADALBERTO||19450409|M|||8753 APPERSON ST^^SUNLAND^CA^91040||(818)429-5631|||||000016715153|572458313
ORC|NW||||||^^^|||||^^^||||||L7173SB00037^^VHH^Med Surg-6^^|
OBR|1|||CHEM8+
NTE|1|||Sample Type=MIX||20110529130917-04:00
NTE|2|||CPB=Yes||20110529130917-04:00
OBX|1|NM|8625-6^P-R interval^LN||<10|%PCV||N|||F|||||APOC3214||equipment|20120529130917-04:00|MIX
OBX|2|DT|41651-1^date^LN||20110529130917-04:00|mg/dL|||||F|||20150529130917-04:00||APOC3214||||MIX
OBX|3|ST|45541-1^GJU^LN||100500 mm||||||F|||||APOC3214|||20110529130917-04:00|MIX
OBX|4|CE|45541-1^GJU^LN||43434-1^8890 jkjsdfk^BV||||||F|||||APOC3214|||20110529130917-04:00|MIX
NTE|1||DSN=314237||20110529130917-04:00
NTE|2||HCT=LOW||20110529130917-04:00
")

  (spit "test/results/oru.yaml" (clj-yaml.core/generate-string (sut/parse oru {})))

  (spit "test/results/oru-r01-1.yaml" (clj-yaml.core/generate-string (sut/parse (slurp "test/messages/oru-r01-1.hl7") {})))

  )


(comment

  (read-files)

  ;; overrride all files
  (foreach-hl7 [input expected]
               (spit expected (-> input (sut/parse {:extensions extensions :strict? false}) zp/zprint-str)))

  (rewrite-hl7-edn "adt-a04-2.hl7")

  (sut/parse-segment ctx "MSH|^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||")

  )

