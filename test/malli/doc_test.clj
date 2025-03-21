(ns malli.doc-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.global :refer [reg reg-type]]
            [malli.doc :refer [doc]]))

(reg ::a-schema :int)
(reg ::another-schema
       "A documentation string"
       {:a static-meta-map}
       ::a-schema)

(reg ::mutual1 [:maybe [:rec ::mutual2]])
(reg ::mutual2 [:maybe [:ref ::mutual1]])

;TODO figure out mutual recursion
;(m/schema ::mutual1)

(doc ::a-schema)
; -------------------------
; Named Schema
; 
; :int
; 
; Source code:
; (m/reg ::a-schema :int)
(doc ::another-schema)
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

(reg-type ::any-alias (m/-any-schema))

(m/validator ::any-alias)
; => any?

(doc ::any-alias)
;TODO
; Schema constructor
; {:file "/Users/ambrose/Projects/malli-local-dev/register-macro/test/malli/doc_test.cljc", :ns malli.doc-test, :line 15, :column 1, :form (m/reg-type :malli.doc-test/any-alias (m/-any-schema)), :schema-form (m/-any-schema), :into-schema true}

(m/schema ::any-alias)
(m/schema ::another-schema)
