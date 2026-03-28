(ns malli.shrinker-test
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
