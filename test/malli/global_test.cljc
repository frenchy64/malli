(ns malli.global-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]
            #?(:clj [malli.test-macros :refer [when-env]])))

(m/reg ::ping
       "The ping side"
       [:maybe [:tuple [:= "ping"] [:ref {:lazy true} ::pong]]])

(m/reg ::pong
       "The pong side"
       [:maybe [:tuple [:= "pong"] [:ref ::ping]]])

(deftest mutually-recursive-globals-test
  (is (m/validator ::ping))
  (is (mg/sample ::ping)))
