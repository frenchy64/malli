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
            [malli.global :refer [reg]]
            #?(:clj [malli.test-macros :refer [when-env]])))

(reg ::list
     "A list of ints"
     [:maybe [:tuple int? [:rec ::list]]])

(reg ::ping
     "The ping side"
     [:maybe [:tuple [:= "ping"] [:rec ::pong]]])

(reg ::pong
     "The pong side"
     [:maybe [:tuple [:= "pong"] [:rec ::ping]]])

(deftest mutually-recursive-globals-test
  (is (m/validator ::ping))
  (is (m/form (m/deref ::ping)))
  (is (mg/sample ::ping))
  (is (mg/sample ::pong))
  (is (mg/sample ::list)))
