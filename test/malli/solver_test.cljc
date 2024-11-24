(ns malli.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.solver :as solver]))

(deftest solve-test
  (is (= [{}] (solver/solve :any nil)))
  (is (= [{:type :number, :max-number -1, :min-number -1}] (solver/solve [:and [:>= -1] [:<= -1]] nil)))
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
