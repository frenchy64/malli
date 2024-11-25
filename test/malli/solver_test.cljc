(ns malli.solver-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
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

(deftest solver-map-of-test
  (is (= '({:type :map, :open-map false}) (solver/solve [:map-of :any :any])))
  (is (= '({:type :map, :keys [{:type :int}], :open-map false}) (solver/solve [:map-of [:and :any :int] :any])))
  (is (= '({:type :map, :vals [{:type :int}] :open-map false}) (solver/solve [:map-of :any [:and :any :int]]))))

(deftest solver-map-test
  (is (= [{:type :map, :open-map true}] (solver/solve :map)))
  (is (= [{:type :map, :keyset {:a :present} :get {:a [{:type :int}]} :min-count 1 :open-map true}]
         (solver/solve [:map [:a :int]])))
  (is (= [{:type :map, :keyset {:a :optional} :get {:a [{:type :int}]} :open-map true}]
         (solver/solve [:map [:a {:optional true} :int]])
         (solver/solve [:map [:a {:optional true} :int]
                        [::m/default [:map]]])))
  (is (= [{:type :map, :open-map true}]
         (solver/solve [:map])
         (solver/solve [:map {:closed false}])
         (solver/solve [:map [::m/default [:map]]])
         (solver/solve [:map {:closed false} [::m/default [:map]]])
         (solver/solve [:map {:closed true} [::m/default [:map]]])))
  (is (= [{:type :map, :open-map false}]
         (solver/solve [:map [::m/default [:map-of :any :any]]])
         (solver/solve [:map {:closed true} [::m/default [:map-of :any :any]]])
         (solver/solve [:map [::m/default [:map {:closed true}]]])))
  (is (= [{:type :map, :get {:a [{:type :int}]}, :keyset {:a :optional}, :open-map false
           :default-keys [{:type :int}] :default-vals [{:type :int}]}]
         (solver/solve [:map [:a {:optional true} :int]
                        [::m/default
                         [:map-of :int :int]]])))
  (is (solver/solve [:map [:a {:optional true} :int]
                     [::m/default
                      [:map [:b {:optional true} :int]]]]))
  (is (solver/solve [:and
                     [:map [:a {:optional true} :int]
                      [::m/default
                       [:map-of :int :int]]]
                     [:map]]))
  (is (solver/solve [:and [:map-of {:min 10} :any :any] empty?]))
  )
(comment
  (m/validate [:map {:closed true} [:a {:optional true} :int]
               [::m/default
                [:map [:b {:optional true} :int]]]]
              {:a 1
               :b 2
               :c 3})
)
