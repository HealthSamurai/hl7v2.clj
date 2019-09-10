(ns hl7v2.schema.core
  (:require [clj-yaml.core]
            [clojure.java.io :as io]))


(def *schema (atom nil))

(defn load-yaml [nm]
  (if-let [f (io/resource nm)] 
    (clj-yaml.core/parse-string (slurp (.getPath f)) :keywords true)
    (throw (Exception. (str nm " not found")))))

(defn schema []
  (if-let [sch  @*schema]
    sch
    (reset!
     *schema
     {:types    (load-yaml "types.yaml")
      :segments (load-yaml "segments.yaml")
      :messages (load-yaml "messages.yaml")})))

(comment

  (def sch (schema))

  )

