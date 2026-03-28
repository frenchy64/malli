(ns malli.shrinker-test
  (:refer-clojure :exclude [sort])
  (:require [malli.shrinker :as ms]
            [malli.core :as m]
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
         (mapv #(update % :vals sort) (deconstruct [:map-of :int :boolean] {1 true 2 false}))))
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

(defn sort
  ([?schema vs] (sort ?schema vs nil))
  ([?schema vs opts]
   (let [s (m/schema ?schema opts)
         valid? (m/validator s)
         _ (assert (every? valid? vs))
         res (vec (ms/sort s vs opts))]
     (dotimes [_ 10]
       (is (= res (ms/sort s (shuffle vs) opts))))
     (dotimes [_ 10]
       (is (= res (ms/sort-by s identity (shuffle vs) opts))))
     res)))

(deftest sort-test
  (is (= [0 10 -10] (sort :int [0 10 -10])))
  (is (= [0 -9 10] (sort :int [0 10 -9])))
  (is (= [0 1 -1] (sort :int [0 1 -1])))
  (is (= [false true] (sort :boolean [false true])))
  (is (= [true false] (sort [:enum true false] [true false])))
  (is (= [false true] (sort [:enum false true] [true false])))
  (is (= [{} {1 true} {1 true}]
         (sort [:map-of :int :boolean] [{} {1 true} {1 true}])))
  (is (= [{} {1 false} {1 true}]
         (sort [:map-of :int :boolean] [{} {1 false} {1 true}])))
  (is (= [{} {0 true} {1 false}]
         (sort [:map-of :int :boolean] [{} {0 true} {1 false}])))
  (is (= [nil nil {} {0 true} {1 false}]
         (sort [:maybe [:map-of :int :boolean]] [nil {} nil {0 true} {1 false}])))
  (is (= '[a aa ab abc] (sort :symbol '[abc aa a ab])))
  (is (= '[:a :aa :ab :abc] (sort :keyword '[:abc :aa :a :ab])))
  (is (= '[:a :ab :abc aa] (sort [:orn [:k :keyword] [:v :symbol]] '[:abc aa :a :ab])))
  (is (= '[aa :a :ab :abc] (sort [:orn [:k :symbol] [:v :keyword]] '[:abc aa :a :ab])))
  (is (= [0 1 2 5] (sort [:schema {:registry {::int :int}} ::int] [0 1 5 2])))
  )

(deftest compare-test
  (is-smaller? :int 0 10)
  (is-smaller? :int 0 -10)
  (is-smaller? :int 1 -1)
  (is-smaller? :int 10 -10)
  (is-equal? [:tuple] [] [])
  (is-smaller? [:tuple :int] [0] [10])
  (is-smaller? [:tuple :int] [0] [10])
  (is-equal? [:map-of :int :boolean] {} {})
  (is-smaller? [:map-of :int :boolean] {} {1 true})
  (is-smaller? [:map-of :int :boolean] {1 true} {-1 true}))
