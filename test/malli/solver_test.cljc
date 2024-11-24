(ns malli.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.solver :as solver]))

(deftest solve-test
  (is (= [{}] (solver/solve :any nil)))
  (is (= [{:type :number, :max-range -1, :min-range -1}] (solver/solve [:and [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :int, :max-range -1, :min-range -1}] (solver/solve [:and :int [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :int, :max-range -1, :min-range -1}] (solver/solve [:and [:int {:gen/min -1}] [:>= -1] [:<= -1]] nil)))
  (is (= [{:type :int, :max-range 5, :min-range -5}] (solver/solve [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] nil)))
  (is (= [{:type :int, :max-range 5, :min-range -5}] (solver/solve [:and [:int {:gen/min -2}] [:>= -5] [:<= 5]] {::mode :gen})))
  (is (= [{:type :number, :min-range -5} {:type :number, :max-range 5}] (solver/solve [:or [:>= -5] [:<= 5]] nil)))
  (is (= [{:type :int, :min-range -5} {:type :int, :max-range 5}] (solver/solve [:and :int [:or [:>= -5] [:<= 5]]] {:gen true})))
  (is (= [{:type :int, :max-range -5, :min-range -5} {:type :int, :max-range 5, :min-range 5}]
         (solver/solve [:and :int [:or
                                   [:and [:<= -5] [:>= -5]]
                                   [:and [:<= 5] [:>= 5]]]]
                       nil))))
  
