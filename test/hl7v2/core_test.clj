(ns hl7v2.core-test
  (:require [hl7v2.core :as sut]
            [clj-yaml.core]
            [clojure.test :refer :all]))

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

  (is (= (dissoc (sut/separators msg) :segment)
         {:field \|,
          :component \^,
          :subcomponet \&,
          :repetition \~,
          :escape \\}))

  (pr-str msg)

  (println (clj-yaml.core/generate-string (sut/parse msg {})))

  )

