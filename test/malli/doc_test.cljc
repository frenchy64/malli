(ns malli.doc-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.doc :as mdoc]))

(m/reg ::a-schema :int)
(m/reg ::another-schema
       "A documentation string"
       {:a static-meta-map}
       :boolean)

(m/doc ::another-schema)
; -------------------------
; Named Schema
; 
; A documentation string
; 
; :bool
; 
; Source code:
; (m/reg ::another-schema
;        "A documentation string"
;        {:a static-meta-map}
;        :bool)

(m/reg-ctor ::any-alias (m/-any-schema))

(m/doc ::any-alias)
; Schema constructor
; {:file "/Users/ambrose/Projects/malli-local-dev/register-macro/test/malli/doc_test.cljc", :ns malli.doc-test, :line 15, :column 1, :form (m/reg-ctor :malli.doc-test/any-alias (m/-any-schema)), :schema-form (m/-any-schema), :into-schema true}

(m/schema ::any-alias)
(m/schema ::another-schema)
