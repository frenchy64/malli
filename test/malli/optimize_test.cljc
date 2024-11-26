;; demo (not included in jar)
(ns malli.optimize-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.optimize :as optimize :refer [validate validator]]
            [malli.solver :as solver]))

(deftest optimize-validator-test
  (is (not ((optimize/-solution-validator {:type :int} nil) nil)))
  (doseq [[validator validate] [[validator validate]
                                [m/validator m/validate]]]
    (testing (pr-str validate)
      (validator number?)
      (is (validate [:and number? [:<= 10]] 0))
      (is (not (validate [:and number? [:<= 10]] 20)))
      (is (validate [:int {:gen/max 10}] 20))
      (is (not (validate [:int {:max 10}] 20)))
      (is (validate :map {}))
      (is (validate [:map [:a :int]] {:a 1}))
      (is (not (validate [:map [:a :int]] {})))
      (is (validate [:map [:a :int]
                     [::m/default
                      [:map-of :int :int]]]
                    {:a 1 1 2}))
      (is (not (validate [:map [:a :int]
                          [::m/default
                           [:map-of :int :int]]]
                         {:a 1 1 nil})))
      (is (not (validate :int nil))))))
