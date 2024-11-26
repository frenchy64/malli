;; demo (not included in jar)
(ns malli.optimize-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.impl.util :as miu]
            [malli.optimize :as optimize :refer [validate validator
                                                 explain explainer]]
            [malli.solver :as solver]))

(deftest optimize-validator-test
  (is (not ((optimize/-solution-validator {:type :int} nil) nil)))
  (doseq [[kind validator validate] [[:optimize validator validate]
                                     [:core m/validator m/validate]]]
    (testing (case kind :core "malli.core" :optimize "malli.optimize")
      (is (true? ((validator number?) 1)))
      (is (false? ((validator number?) nil)))
      (is (true? (validate [:and number? [:<= 10]] 0)))
      (is (false? (validate [:and number? [:<= 10]] 20)))
      (is (true? (validate [:int {:gen/max 10}] 20)))
      (is (false? (validate [:int {:max 10}] 20)))
      (is (true? (validate :map {})))
      (is (true? (validate [:map [:a :int]] {:a 1})))
      (is (false? (validate [:map [:a :int]] {})))
      (is (true? (validate [:map [:a :int]
                            [::m/default
                             [:map-of :int :int]]]
                           {:a 1 1 2})))
      (is (false? (validate [:map [:a :int]
                             [::m/default
                              [:map-of :int :int]]]
                            {:a 1 1 nil})))
      (is (false? (validate :int nil))))))

(deftest optimize-explainer-test
  (doseq [[kind explainer explain] [[:optimize explainer explain]
                                    [:core m/explainer m/explain]]]
    (testing (case kind :core "malli.core" :optimize "malli.optimize")
      (is (nil? (explain :int 1)))
      ;;hmm by simplifying do we destroy the original schema structure?
      (when (= kind :optimize)
        (is (= {:schema :int, :value nil, :errors [{:path [], :in [], :schema nil, :value nil}]}
               (-> (explain :int nil)
                   (update :schema m/form))))))))
