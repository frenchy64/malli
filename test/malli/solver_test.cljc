(ns malli.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.solver :as solver]))

(deftest solve-number-test
  (is (= [{}] (solver/solve :any nil)))
  (is (= [{:type :number, :max-number -1, :min-number -1}] (solver/solve [:and [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :number, :max-number 0, :min-number 0}] (solver/solve zero?)))
  (is (= [{:type :int, :max-number -1, :min-number -1}] (solver/solve [:and :int [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :int, :max-number -1, :min-number -1}] (solver/solve [:and [:int {:gen/min -1}] [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :int, :max-number 5, :min-number -5}] (solver/solve [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] nil)))
  (is (= [{:type :int, :max-number 5, :min-number -5}] (solver/solve [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] {::mode :gen})))
  (is (= [{:type :number, :min-number -5} {:type :number, :max-number 5}] (solver/solve [:or [:>= -5] [:<= 5]] nil)))
  (is (= [{:type :int, :min-number -5} {:type :int, :max-number 5}] (solver/solve [:and :int [:or [:>= -5] [:<= 5]]] {:gen true})))
  (is (= [{:type :int, :max-number -5, :min-number -5} {:type :int, :max-number 5, :min-number 5}]
         (solver/solve [:and :int [:or
                                   [:and [:<= -5] [:>= -5]]
                                   [:and [:<= 5] [:>= 5]]]]
                       nil))))

(deftest solve-coll-test
  (is (= [{:type :coll}] (solver/solve coll?)))
  (is (= [{:type :counted, :min-count 0, :max-count 0} {:type :seqable, :min-count 0, :max-count 0}]
         (solver/solve empty?)))
  (is (= [{:type :indexed}] (solver/solve indexed?)))
  (is (= [{:type :map}] (solver/solve map?)))
  (is (= [{:type :seq}] (solver/solve seq?)))
  (is (= [{:type :seqable}] (solver/solve seqable?)))
  (is (= [{:type :sequential}] (solver/solve sequential?)))
  (is (= [{:type :set}] (solver/solve set?)))
  (is (= [{:type :vector}] (solver/solve vector?))))
