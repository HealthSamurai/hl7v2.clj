(ns hl7v2.schema.types
  (:refer-clojure :exclude [format])
  (:require [java-time.format :as fmt]
            [java-time :refer [zoned-date-time local-date-time local-date format]]))

(def with-tz-formatter
  [(partial zoned-date-time (fmt/formatter "yyyyMMddHHmmssz"))
   (partial format (get fmt/predefined-formatters "iso-offset-date-time"))])

(def without-tz-formatter
  [(partial local-date-time (fmt/formatter "yyyyMMddHHmmss"))
   (partial format (get fmt/predefined-formatters "iso-local-date-time"))])

(def date-formatter
  [(partial local-date (fmt/formatter "yyyyMMdd"))
   (partial format (get fmt/predefined-formatters "iso-local-date"))])

(defn reformat [[parser printer] s]
  (-> s parser printer))

(def format-datetime-with-tz (partial reformat with-tz-formatter))
(def format-datetime-without-tz (partial reformat without-tz-formatter))
(def format-date (partial reformat date-formatter))

(defn format-datetime [s]
  (case (count s)
    20 (format-datetime-with-tz s)
    14 (format-datetime-without-tz s)
    8 (format-date s)
    s))

(def typed-formatters
  {:DTM format-datetime})
