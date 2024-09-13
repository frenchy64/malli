(ns malli.constraint.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.constraint.solver :as mcs]))

(deftest -conj-number-constraints-test
  (is (= [{:max-count 10 :min-count 1}]
         (mcs/-conj-number-constraints
           [{:max-count 10} {:min-count 1}])))
  (is (= [] (mcs/-conj-number-constraints [{:max-count 1} {:min-count 10}])))
  (is (= [] (mcs/-conj-number-constraints [{:max-count 1} {:min-count 10}]))))

(deftest -conj-solutions-test
  (is (= [{}] (mcs/-conj-solutions)))
  (is (= [{:max-count 2}] (mcs/-conj-solutions '({:max-count 2}))))
  (is (= [{:max-count 0}]
         (mcs/-conj-solutions '({:max-count 2})
                              '({:max-count 0}))))
  (is (= [{:min-count 2}]
         (mcs/-conj-solutions '({:min-count 2})
                              '({:min-count 0})))))

(deftest -constraint-solutions-test
  (is (= [{:min-count 0, :max-count 2}]
         (mcs/-constraint-solutions
           [:and [:min 0] [:max 2]] :string nil))))
