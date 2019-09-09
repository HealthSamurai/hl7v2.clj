(ns hl7v2.schema.core
  (:require [clj-yaml.core]
            [clojure.java.io :as io]))


(def *schema (atom nil))
(defn load-yaml [nm]
  (clj-yaml.core/parse-string (slurp (.getPath (io/resource nm))) {:keywords true}))

(defn schema []
  (if-let [sch  @schema]
    sch
    {:types (load-yaml "types.yaml")
     :segments (load-yaml "segments.yaml")
     :messages (load-yaml "messages.yam")}))

(comment

  (def sch (schema))

  )

