(ns malli.doc-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.doc :as mdoc]))

(m/reg ::a-schema :int)
(m/reg ::another-schema
       "A documentation string"
       {:a static-meta-map}
       :bool)

(m/doc ::another-schema)

(m/reg-ctor ::any-alias (m/-any-schema))

(m/doc ::any-alias)
