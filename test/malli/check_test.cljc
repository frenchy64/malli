(ns malli.check-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def good-identities [identity
                      (fn [a] a)
                      (fn [a] (identity a))])
(def bad-identities [(fn [_] nil)
                     (fn [a] (when (uuid? a) a))])

(def identity-spec (m/all [a] [:=> [:cat a] a]))

(deftest identity-test
  (testing "good-identities"
    (doseq [[i f] (map-indexed vector good-identities)]
      (testing i
        (is (nil? (mg/check identity-spec f))))))
  (testing "bad-identities"
    (doseq [[i f] (map-indexed vector bad-identities)]
      (testing i
        (is (mg/check identity-spec f {::mg/all-iterations 100}))))))
