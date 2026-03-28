(ns malli.shrinker-test
  (:refer-clojure :exclude [sort])
  (:require [malli.shrinker :as ms]
            [malli.core :as m]
            [clojure.core :as c]
            [clojure.test :refer [deftest is]]))

(def Expr
  [:schema {:registry {::Expr [:orn
                               [:Atomic [:maybe [:or :int :boolean]]]
                               [:Ref :symbol]
                               [:App [:tuple
                                      [:ref ::Expr]
                                      [:ref ::Expr]]]
                               [:Let [:tuple
                                      [:enum 'let]
                                      [:tuple :symbol [:ref ::Expr]]
                                      [:ref ::Expr]]]]}}
   ::Expr])

(comment
  (m/parse Expr '[inc 42])
  (m/parse Expr '[let [a 1] [inc 42]])
  (m/parse [:orn
            [:left [:tuple :int :boolean]]
            [:right [:map-of :int :boolean]]]
           [1 true])
  (m/children
    [:orn
     [:left [:tuple :int :boolean]]
     [:right [:map-of :int :boolean]]])
  (m/parse [:sequential :int] [1 2 3])
  (m/parse [:or :int :boolean] 2)
  (m/parse [:map [:a :int] [:b :boolean]] {:a 1 :b true})
  )

(defn deconstruct
  ([schema value] (deconstruct schema value nil))
  ([schema value opts] (mapv #(update % :schema m/form) (ms/deconstruct schema value opts))))
(defn deconstruct-atomic [schema value] (deconstruct schema value {::ms/deconstruct-atomic true}))

(deftest deconstruct-test
  (is (= [{:schema :int, :path [0], :vals [1 2 3]}]
         (deconstruct [:sequential :int] [1 2 3])
         (deconstruct [:seqable :int] [1 2 3])
         (deconstruct [:every :int] [1 2 3])))
  (is (= [{:schema :int, :path [0], :vals #{1 2 3}}]
         (deconstruct [:set :int] #{1 2 3})))
  (is (= [{:schema :int, :path [0], :vals [1]}
          {:schema :boolean, :path [1], :vals [true]}
          {:schema :string, :path [2], :vals ["a"]}]
         (deconstruct [:tuple :int :boolean :string] [1 true "a"])))
  (is (= [{:schema :int, :path [0], :vals [1 2]}
          {:schema :boolean, :path [1], :vals [false true]}]
         (mapv #(update % :vals c/sort) (deconstruct [:map-of :int :boolean] {1 true 2 false}))))
  (is (= [{:schema :any, :vals [1 [true] 2 false]}]
         (deconstruct :any [1 [true] 2 false])))
  (is (= [{:schema :any, :path [], :vals {1 [true] 2 false}}]
         (deconstruct :any {1 [true] 2 false})))
  (is (= [{:schema :int, :val 1}
          {:schema :boolean, :val true}]
         (deconstruct [:orn
                  [:left [:tuple :int :boolean]]
                  [:right [:map-of :int :boolean]]]
                 [1 true])))
  (is (= [] (deconstruct :string "asdf1234"))))

(deftest deconstruct-atomic-test
  (is (= [{:schema :string, :path [], :vals ["asdf" "1234" "sdf1234" "asdf123"]}]
         (deconstruct-atomic :string "asdf1234")))
  (is (= [] (deconstruct-atomic :string "")))
  (is (= [{:schema :string, :path [], :vals ["" "a"]}]
         (deconstruct-atomic :string "a")))
  (is (= [{:schema :string, :path [], :vals ["a" "b"]}]
         (deconstruct-atomic :string "ab")))
  (is (= [{:schema :string, :path [], :vals ["a" "bc" "bc" "ab"]}]
         (deconstruct-atomic :string "abc")))
  (is (= [{:schema :string, :path [], :vals ["ab" "cd" "bcd" "abc"]}]
         (deconstruct-atomic :string "abcd"))))

(deftest shrink-test
  (is (= (ms/shrink Expr '[let [a 1] [inc 42]])
         '[1 [inc 42]]))
  (is (= (ms/shrink Expr '[inc 42])
         '[inc 42])))

(defn is-smaller?
  ([?schema left right] (is-smaller? ?schema left right nil))
  ([?schema left right opts]
   (let [s (m/schema ?schema opts)
         valid? (m/validator s)]
     (is (valid? left))
     (is (valid? right))
     (is (ms/smaller? s left right opts))
     (is (ms/larger? s right left opts)))))

(defn is-equal?
  ([?schema left right] (is-equal? ?schema left right nil))
  ([?schema left right opts]
   (let [s (m/schema ?schema opts)
         valid? (m/validator s)]
     (is (valid? left))
     (is (valid? right))
     (is (= :equal (ms/compare s left right opts)))
     (is (= :equal (ms/compare s right left opts))))))

(defn is-sort
  ([?schema expected] (is-sort ?schema expected nil))
  ([?schema expected opts]
   (let [s (m/schema ?schema opts)
         valid? (m/validator s)
         _ (assert (every? valid? expected))
         res (some-> (ms/sort s expected opts) vec)]
     (when (is (= expected res))
       (doseq [_ (range 10)
               :let [vs (some-> expected shuffle)]]
         (is (= expected (ms/sort s vs opts))
             (pr-str (list 'ms/sort (m/form s) vs))))
       (doseq [_ (range 10)
               :let [vs (some-> expected shuffle)]]
         (is (= expected (ms/sort-by s identity vs opts))
             (pr-str (list 'ms/sort-by (m/form s) vs))))
       res))))

(comment
  (ms/sort :int [0 10 -10])
  (ms/sort :int [10 -10 0])
  (ms/sort :int [-10 0 10])
  )

(deftest sort-test
  (is-sort :int [0 10 -10])
  (is-sort :int [0 -9 10])
  (is-sort :int [0 1 -1])
  (is-sort :boolean [false true])
  (is-sort [:enum true false] [true false])
  (is-sort [:enum false true] [false true])
  (is-sort [:map-of :int :boolean] [{} {1 true} {1 true}])
  (is-sort [:map-of :int :boolean] [{} {1 false} {1 true}])
  (is-sort [:map-of :int :boolean] [{} {0 true} {1 false}])
  (is-sort [:maybe [:map-of :int :boolean]] [nil nil {} {0 true} {1 false}])
  (is-sort :symbol '(a aa ab abc))
  (is-sort :keyword [:a :aa :ab :abc])
  (is-sort [:orn [:k :keyword] [:v :symbol]] '[:a :ab :abc aa])
  (is-sort [:orn [:k :symbol] [:v :keyword]] '[aa :a :ab :abc])
  (is-sort [:schema {:registry {::int :int}} ::int] [0 1 2 5])
  #_(is-sort [:map [:a :int] [:b :boolean]] [{:a 1 :b false} {:a 1 :b true}])
  )

(deftest compare-test
  (is-smaller? :int 0 10)
  (is-smaller? :int 0 -10)
  (is-smaller? :int 1 -1)
  (is-smaller? :int 10 -10)
  (is-equal? [:tuple] [] [])
  (is-smaller? [:tuple :int] [0] [10])
  (is-equal? [:map-of :int :boolean] {} {})
  (is-smaller? [:map-of :int :boolean] {} {1 true})
  (is-smaller? [:map-of :int :boolean] {1 true} {-1 true}))
