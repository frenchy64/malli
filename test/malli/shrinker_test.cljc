(ns malli.shrinker-test
  (:refer-clojure :exclude [sort])
  (:require [malli.shrinker :as ms]
            [malli.core :as m]
            [clojure.core :as c]
            [clojure.test :refer [deftest is testing]]))

(def Address
  [:map
   [:id :string]
   [:tags [:set :keyword]]
   [:address
    [:map
     [:street :string]
     [:city :string]
     [:zip :int]
     [:lonlat [:tuple :double :double]]]]])

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
(defn divide
  ([schema value] (divide schema value nil))
  ([schema value opts] (mapv #(update % :schema m/form) (ms/divide schema value opts))))

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

#_ ;;TODO
(deftest divide-test
  (is (= [{:schema :string, :path [], :vals ["asdf" "1234" "sdf1234" "asdf123"]}]
         (divide :string "asdf1234")))
  (is (= [] (divide :string "")))
  (is (= [{:schema :string, :path [], :vals ["" "a"]}]
         (divide :string "a")))
  (is (= [{:schema :string, :path [], :vals ["a" "b"]}]
         (divide :string "ab")))
  (is (= [{:schema :string, :path [], :vals ["a" "bc" "bc" "ab"]}]
         (divide :string "abc")))
  (is (= [{:schema :string, :path [], :vals ["ab" "cd" "bcd" "abc"]}]
         (divide :string "abcd"))))

(defn shrink [?schema v]
  (let [schema (m/schema ?schema)
        valid? (m/validator schema)]
    (is (valid? v))
    (mapv :value (ms/shrink schema v))))

(deftest shrink-test
  (is (= (shrink Expr '[let [a 1] [inc 42]])
         '([inc 42]
           inc
           42
           1)))
  (is (= (shrink Expr '[inc [inc [inc 42]]])
         '([inc [inc 42]]
           [inc 42]
           inc
           42)))
  (is (= (shrink Expr '[inc 42])
         '(inc 42))))

(defn is-smaller?
  ([?schema left right] (is-smaller? ?schema left right nil))
  ([?schema left right opts]
   (let [s (m/schema ?schema opts)
         valid? (m/validator s)]
     (is (valid? left))
     (is (valid? right))
     (is (ms/smaller? s left right opts))
     (is (= :smaller (ms/compare s left right opts)))
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
  (is-sort Expr [['let ['a 1] 'a]
                 ['let ['abc 1] 'abc]])
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
  (is-smaller? [:map-of :int :boolean] {1 true} {-1 true})
  (is-smaller? Expr 'a ['a 'a])
  (is-smaller? Expr ['a 'a] ['let ['a 1] 'a])
  (is-smaller? Expr
               ['let ['a 1] 'a]
               ['let ['a 1] 'b])
  (is-smaller? Expr
               ['let ['a 1] 'a]
               ['let ['a 1] ['let ['a 1] 'a]])
  ;; ideal comparison order for
  ;; ['let [B0 I0] E0] vs. ['let [B1 I1] E1]
  ;; largest depth difference
  ;; 1. (compare I0 I1)
  ;; 2. (compare E0 E1)
  ;; 3. (compare B0 B1)
  (is-smaller? Expr
               ['let ['b 1] 'a]
               ['let ['a 1] ['let ['a 1] 'a]])
  (is-smaller? Expr
               ['let ['a 1] 'a]
               ['let ['b 1] ['let ['a 1] 'a]])
  (is-smaller? Expr
               ['let ['a 1] ['a 'a]]
               ['let ['b 1] ['let ['a 1] 'a]])
  (is-smaller? [:sequential :int] [0 1 2] [0 1 2 4 5 6])
  (is-smaller? [:sequential [:sequential :int]]
               [[0 1 2]]
               [[0 1 2 4 5 6]])
  (is-smaller? [:sequential [:sequential :int]]
               [[0 1 2 4 5 6]]
               [[0 1 2] [4 5 6]])
  (is-smaller? [:set [:sequential :int]]
               #{[] [1]}
               #{[] [1]} #{[1 2]})
  (is-smaller? [:set :int] #{0} #{1 2})
  (is-smaller? [:set [:set [:sequential :int]]]
               #{#{[] [1]}}
               #{#{[] [1]} #{[1 2]}})
  (is-smaller? [:set [:set [:sequential :int]]]
               #{}
               #{#{}})
  (is-equal? [:set [:set [:sequential :int]]]
             #{}
             #{})
  (is-equal? [:set [:set [:sequential :int]]]
             #{#{}}
             #{#{}})
  (is-smaller? [:set [:set [:sequential :int]]]
               #{#{}}
               #{#{[]}})
  (is-smaller? [:set [:set [:sequential :int]]]
               #{#{[] [1]}}
               #{#{[] [2]}}))

(def Let (m/-get (m/deref-all Expr) :Let nil))

(comment
  (do Let)
  (paths Let)
  ;=>
  [[0]]
)

(defn diff [schema left right]
  (let [schema (m/schema schema)
        valid? (m/validator schema)]
    (assert (valid? left))
    (assert (valid? right))
    (some->> (ms/diff schema left right)
             (mapv #(update % :schema m/form)))))

(deftest diff-test
  (is (= nil (diff :int 0 0)))
  (is (= [{:result :larger, :schema :int, :path [], :in [], :left -1, :right 1}]
         (diff :int -1 1)))
  (is (= [{:result :smaller, :schema :int, :path [], :in [], :left 1, :right -1}]
         (diff :int 1 -1)))
  (is (= nil (diff [:maybe :int] nil nil)))
  (is (= [{:result :smaller,
           :schema [:maybe :int],
           :path [],
           :in [],
           :left nil,
           :right 1}]
         (diff [:maybe :int] nil 1)))
  (is (= [{:result :larger,
           :schema [:maybe :int],
           :path [],
           :in [],
           :left 1,
           :right nil}]
         (diff [:maybe :int] 1 nil)))
  (is (= nil (diff [:maybe :int] 1 1)))
  (is (= [{:result :larger, :schema :int, :path [0], :in [], :left -1, :right 1}]
         (diff [:maybe :int] -1 1)))
  (is (= [{:result :smaller, :schema :int, :path [0], :in [], :left 1, :right -1}]
         (diff [:maybe :int] 1 -1)))
  (is (= nil (diff [:tuple :int] [0] [0])))
  (is (= [{:result :smaller,
           :schema :int,
           :path [0],
           :in [0],
           :left 0,
           :right 1}]
         (diff [:tuple :int] [0] [1])))
  (is (= '[{:result :smaller,
            :schema
            [:orn
             [:Atomic [:maybe [:or :int :boolean]]]
             [:Ref :symbol]
             [:App
              [:tuple
               [:ref :malli.shrinker-test/Expr]
               [:ref :malli.shrinker-test/Expr]]]
             [:Let
              [:tuple
               [:enum let]
               [:tuple :symbol [:ref :malli.shrinker-test/Expr]]
               [:ref :malli.shrinker-test/Expr]]]],
            :path [0 0 :Let 2 0],
            :in [2],
            :left a,
            :right [let [a 1] a]}]
         (diff Expr
               ['let ['a 1] 'a]
               ['let ['a 1] ['let ['a 1] 'a]])))
  (is (= '[{:result :larger,
            :schema :symbol,
            :path [0 0 :Let 1 0],
            :in [1 0],
            :left b,
            :right a}
           {:result :smaller,
            :schema
            [:orn
             [:Atomic [:maybe [:or :int :boolean]]]
             [:Ref :symbol]
             [:App
              [:tuple
               [:ref :malli.shrinker-test/Expr]
               [:ref :malli.shrinker-test/Expr]]]
             [:Let
              [:tuple
               [:enum let]
               [:tuple :symbol [:ref :malli.shrinker-test/Expr]]
               [:ref :malli.shrinker-test/Expr]]]],
            :path [0 0 :Let 2 0],
            :in [2],
            :left a,
            :right [let [a 1] a]}]
         (diff Expr
               ['let ['b 1] 'a]
               ['let ['a 1] ['let ['a 1] 'a]]))))

(defn leaves [schema v]
  (let [schema (m/schema schema)
        valid? (m/validator schema)]
    (assert (valid? v))
    (mapv #(update % :schema m/form) (ms/leaves schema v))))

(defn leaf-paths [schema v] (mapv :path (leaves schema v)))

(deftest leaves-test
  (is (= [[]] (leaf-paths :int 0)))
  (is (= [[0]] (leaf-paths [:tuple :int] [0])))
  (is (= [[]] (leaf-paths [:tuple] [])))
  (is (= [[0 0]] (leaf-paths [:tuple [:tuple :int]] [[0]])))
  (is (= [[0 0] [0 1]] (leaf-paths [:tuple [:tuple :int [:enum :a]]] [[0 :a]])))
  (is (= [[0 0 :Ref]] (leaf-paths Expr 'a)))
  (is (= [[0 [3 :Let] 0]
          [0 [3 :Let] 1 0]
          [0 [3 :Let] 1 1 0 [0 :Atomic] 0 0]
          [0 [3 :Let] 2 0 [3 :Let] 0]
          [0 [3 :Let] 2 0 [3 :Let] 1 0]
          [0 [3 :Let] 2 0 [3 :Let] 1 1 0 [0 :Atomic] 0 0]
          [0 [3 :Let] 2 0 [3 :Let] 2 0 [1 :Ref]]]
         (leaf-paths Expr ['let ['a 1] ['let ['a 1] 'a]])))
  (is (= '[{:schema [:enum let], :id 2, :path [0 0 [3 :Let] 0], :in [0], :value let}
           {:schema :symbol, :id 4, :path [0 0 [3 :Let] 1 0], :in [1 0], :value b}
           {:schema :int, :id 7, :path [0 0 [3 :Let] 1 1 0 [0 :Atomic] 0 0], :in [1 1], :value 1}
           {:schema :symbol, :id 4, :path [0 0 [3 :Let] 2 0 [1 :Ref]], :in [2], :value a}]
         (leaves Expr ['let ['b 1] 'a])))
  (is (= '[{:schema [:enum let], :id 2, :path [0 0 [3 :Let] 0], :in [0], :value let}
           {:schema :symbol, :id 4, :path [0 0 [3 :Let] 1 0], :in [1 0], :value a}
           {:schema :int, :id 7, :path [0 0 [3 :Let] 1 1 0 [0 :Atomic] 0 0], :in [1 1], :value 1}
           {:schema [:enum let], :id 2, :path [0 0 [3 :Let] 2 0 [3 :Let] 0], :in [2 0], :value let}
           {:schema :symbol, :id 4, :path [0 0 [3 :Let] 2 0 [3 :Let] 1 0], :in [2 1 0], :value a}
           {:schema :int, :id 7, :path [0 0 [3 :Let] 2 0 [3 :Let] 1 1 0 [0 :Atomic] 0 0], :in [2 1 1], :value 1}
           {:schema :symbol, :id 4, :path [0 0 [3 :Let] 2 0 [3 :Let] 2 0 [1 :Ref]], :in [2 2], :value a}]
         (leaves Expr ['let ['a 1] ['let ['a 1] 'a]])))
  (is (= [{:schema :int, :id 1, :path [0], :in [0], :value 0}
          {:schema :int, :id 1, :path [0], :in [1], :value 1}
          {:schema :int, :id 1, :path [0], :in [2], :value 2}]
         (leaves [:sequential :int] [0 1 2])))
  (is (= [{:schema :int, :id 2, :path [0 [0 :left]], :in [0], :value 0}
          {:schema :boolean, :id 3, :path [0 [1 :right]], :in [1], :value true}
          {:schema :int, :id 2, :path [0 [0 :left]], :in [2], :value 2}]
         (leaves [:sequential [:orn [:left :int] [:right :boolean]]] [0 true 2])))
  (is (= [{:schema [:sequential :int], :id 0, :path [], :in [], :value []}]
         (leaves [:sequential :int] [])))
  (is (= [{:schema :int, :id 2, :path [0 0], :in [0 0], :value 0}
          {:schema :int, :id 2, :path [0 0], :in [0 1], :value 1}
          {:schema :int, :id 2, :path [0 0], :in [0 2], :value 2}
          {:schema :int, :id 2, :path [0 0], :in [0 3], :value 4}
          {:schema :int, :id 2, :path [0 0], :in [0 4], :value 5}
          {:schema :int, :id 2, :path [0 0], :in [0 5], :value 6}]
         (leaves [:sequential [:sequential :int]] [[0 1 2 4 5 6]])))
  (is (= [{:schema :int, :id 2, :path [0 0], :in [0 0], :value 0}
          {:schema :int, :id 2, :path [0 0], :in [0 1], :value 1}
          {:schema :int, :id 2, :path [0 0], :in [0 2], :value 2}
          {:schema :int, :id 2, :path [0 0], :in [1 0], :value 4}
          {:schema :int, :id 2, :path [0 0], :in [1 1], :value 5}
          {:schema :int, :id 2, :path [0 0], :in [1 2], :value 6}]
         (leaves [:sequential [:sequential :int]] [[0 1 2] [4 5 6]])))
  (is (= [{:schema [:enum :a :b :c], :id 0, :path [], :inner-path [0], :in [], :value :a}]
         (leaves [:enum :a :b :c] :a)))
  (is (= [{:schema :int, :id 1, :path [0], :in [0], :value 1}
          {:schema :int, :id 1, :path [0], :in [1], :value 2}
          {:schema :int, :id 1, :path [0], :in [2], :value 3}]
         (leaves [:set :int] #{1 2 3})))
  (is (= [{:schema [:sequential :int], :id 1, :path [0], :in [0], :value []}
          {:schema :int, :id 2, :path [0 0], :in [1 0], :value 1}
          {:schema :int, :id 2, :path [0 0], :in [2 0], :value 1}
          {:schema :int, :id 2, :path [0 0], :in [2 1], :value 2}]
         (leaves [:set [:sequential :int]] #{[] [1] [1 2]})))
  (is (= [{:schema [:set [:sequential :int]], :id 1, :path [0], :in [0], :value #{}}]
         (leaves [:set [:set [:sequential :int]]] #{#{}})))
  (is (= [{:schema [:sequential :int], :id 2, :path [0 0], :in [0 0], :value []}
          {:schema :int, :id 3, :path [0 0 0], :in [0 1 0], :value 1}
          {:schema :int, :id 3, :path [0 0 0], :in [1 0 0], :value 1}
          {:schema :int, :id 3, :path [0 0 0], :in [1 0 1], :value 2}]
         (leaves [:set [:set [:sequential :int]]] #{#{[] [1]} #{[1 2]}}))))

(defn leaf-complexity [schema v]
  (ms/-leaf-complexity (leaves schema v)))

(deftest leaf-complexity-test
  (is (= 1  (leaf-complexity :int 0)))
  (is (= 1  (leaf-complexity [:sequential :int] [])))
  (is (= 6  (leaf-complexity [:sequential :int] [0 1 2])))
  (is (= 12 (leaf-complexity [:sequential :int] [0 1 2 4 5 6])))
  (is (= 9  (leaf-complexity [:sequential [:sequential :int]] [[0 1 2]])))
  (is (= 18 (leaf-complexity [:sequential [:sequential :int]] [[0 1 2 4 5 6]])))
  (is (= 18 (leaf-complexity [:sequential [:sequential :int]] [[0 1 2] [4 5 6]])))
  (is (= 11 (leaf-complexity [:set [:sequential :int]] #{[] [1] [1 2]}))))

(defn leaf-in-depth [schema v]
  (ms/-leaf-in-depth (leaves schema v)))

(deftest leaf-in-depth-test
  (is (= 0  (leaf-in-depth :int 0)))
  (is (= 0  (leaf-in-depth [:sequential :int] [])))
  (is (= 3  (leaf-in-depth [:sequential :int] [0 1 2])))
  (is (= 6  (leaf-in-depth [:sequential :int] [0 1 2 4 5 6])))
  (is (= 6  (leaf-in-depth [:sequential [:sequential :int]] [[0 1 2]])))
  (is (= 12 (leaf-in-depth [:sequential [:sequential :int]] [[0 1 2 4 5 6]])))
  (is (= 12 (leaf-in-depth [:sequential [:sequential :int]] [[0 1 2] [4 5 6]])))
  (is (= 3 (leaf-in-depth [:set :int] #{1 2 3})))
  (is (= 7 (leaf-in-depth [:set [:sequential :int]] #{[] [1] [1 2]}))))

(defn explode [schema v]
  (let [schema (m/schema schema)
        valid? (m/validator schema)]
    (assert (valid? v))
    (into [] (comp (remove :ref) (map #(update % :schema m/form))) (ms/explode schema v))))

(deftest explode-test
  (is (= [{:schema [:sequential :int], :id 0, :path [], :in [], :value [1 2 3]}
          {:schema :int, :id 1, :path [0], :in [0], :leaf true, :value 1}
          {:schema :int, :id 1, :path [0], :in [1], :leaf true, :value 2}
          {:schema :int, :id 1, :path [0], :in [2], :leaf true, :value 3}]
         (explode [:sequential :int] [1 2 3])))
  (is (= [{:schema [:sequential [:sequential :int]], :id 0, :path [], :in [], :value [[1 2] [3]]}
          {:schema [:sequential :int], :id 1, :path [0], :in [0], :value [1 2]}
          {:schema :int, :id 2, :path [0 0], :in [0 0], :leaf true, :value 1}
          {:schema :int, :id 2, :path [0 0], :in [0 1], :leaf true, :value 2}
          {:schema [:sequential :int], :id 1, :path [0], :in [1], :value [3]}
          {:schema :int, :id 2, :path [0 0], :in [1 0], :leaf true, :value 3}]
         (explode [:sequential [:sequential :int]] [[1 2] [3]])))
  (is (= [{:schema [:set [:sequential :int]], :id 0, :path [], :in [], :value #{[] [1]}}
          {:schema [:sequential :int], :id 1, :path [0], :in [0], :value [], :leaf true}
          {:schema [:sequential :int], :id 1, :path [0], :in [1], :value [1]}
          {:schema :int, :id 2, :path [0 0], :in [1 0], :leaf true, :value 1}]
         (explode [:set [:sequential :int]] #{[] [1]})))
  (is (= [{:schema [:set [:set [:sequential :int]]], :id 0, :path [], :in [], :value #{#{[1 2]} #{[] [1]}}}
          {:schema [:set [:sequential :int]], :id 1, :path [0], :in [0], :value #{[] [1]}}
          {:schema [:sequential :int], :id 2, :path [0 0], :in [0 0], :value [], :leaf true}
          {:schema [:sequential :int], :id 2, :path [0 0], :in [0 1], :value [1]}
          {:schema :int, :id 3, :path [0 0 0], :in [0 1 0], :leaf true, :value 1}
          {:schema [:set [:sequential :int]], :id 1, :path [0], :in [1], :value #{[1 2]}}
          {:schema [:sequential :int], :id 2, :path [0 0], :in [1 0], :value [1 2]}
          {:schema :int, :id 3, :path [0 0 0], :in [1 0 0], :leaf true, :value 1}
          {:schema :int, :id 3, :path [0 0 0], :in [1 0 1], :leaf true, :value 2}]
         (explode [:set [:set [:sequential :int]]] #{#{[] [1]} #{[1 2]}})))
  (is (= [{:schema [:maybe [:sequential [:ref "Cons"]]], :id 0, :path [0 0], :in [], :value []}
          {:schema [:sequential [:ref "Cons"]], :id 1, :path [0 0 0], :in [], :value [], :leaf true}]
         (explode [:schema {:registry {"Cons" [:maybe [:sequential [:ref "Cons"]]]}} "Cons"]
                  [])))
  (is (= [{:schema [:maybe [:sequential [:ref "Cons"]]],
           :id 0, :path [0 0], :in [], :value [[[[nil]]]]}
          {:schema [:sequential [:ref "Cons"]],
           :id 1, :path [0 0 0], :in [], :value [[[[nil]]]]}
          {:schema [:maybe [:sequential [:ref "Cons"]]],
           :id 0, :path [0 0 0 0 0], :in [0], :value [[[nil]]]}
          {:schema [:sequential [:ref "Cons"]],
           :id 1, :path [0 0 0 0 0 0], :in [0], :value [[[nil]]]}
          {:schema [:maybe [:sequential [:ref "Cons"]]],
           :id 0, :path [0 0 0 0 0 0 0 0], :in [0 0], :value [[nil]]}
          {:schema [:sequential [:ref "Cons"]],
           :id 1, :path [0 0 0 0 0 0 0 0 0], :in [0 0], :value [[nil]]}
          {:schema [:maybe [:sequential [:ref "Cons"]]],
           :id 0, :path [0 0 0 0 0 0 0 0 0 0 0], :in [0 0 0], :value [nil]}
          {:schema [:sequential [:ref "Cons"]],
           :id 1, :path [0 0 0 0 0 0 0 0 0 0 0 0], :in [0 0 0], :value [nil]}
          {:schema [:maybe [:sequential [:ref "Cons"]]],
           :id 0, :path [0 0 0 0 0 0 0 0 0 0 0 0 0 0], :in [0 0 0 0], :value nil, :leaf true}
          {:schema [:sequential [:ref "Cons"]],
           :id 1, :path [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0], :in [0 0 0 0], :value nil, :leaf true}]
         (explode [:schema {:registry {"Cons" [:maybe [:sequential [:ref "Cons"]]]}} "Cons"]
                  [[[[nil]]]])))
  (is (= [{:schema :map, :id 0, :path [], :in [], :leaf true, :value {}}]
         (explode [:map] {})))
  (is (= [{:schema [:map [:a :int]], :id 0, :path [], :in [], :value {:a 1}}
          {:schema :int, :id 1, :path [[0 :a]], :in [:a], :leaf true, :value 1}]
         (explode [:map [:a :int]] {:a 1})))
  ;;TODO explode :map-of
  (is (= [{:schema [:map [:a :int] [:malli.core/default [:map-of :int :boolean]]],
           :id 0, :path [], :in [], :value {:a 1, 1 true}}
          {:schema :int, :id 1, :path [[0 :a]], :in [:a], :leaf true, :value 1}]
         (explode [:map [:a :int] [::m/default [:map-of :int :boolean]]] {:a 1 1 true})))
  (is (= [{:schema [:map [:a :int]], :id 0, :path [], :in [], :value {:a 1 :b 2}}
          {:schema :int, :id 1, :path [[0 :a]], :in [:a], :leaf true, :value :a}
          ::FIXME]
         (explode [:map [:a :int]] {:a 1 :b 2})))
  (is (= [{:schema [:tuple :int :boolean], :id 0, :path [], :in [], :value [1 true]}
          {:schema :int, :id 1, :path [0], :in [0], :leaf true, :value 1}
          {:schema :boolean, :id 2, :path [1], :in [1], :leaf true, :value true}]
         (explode [:tuple :int :boolean] [1 true])))
  (is (= [{:schema
           [:map
            [:id :string]
            [:tags [:set :keyword]]
            [:address
             [:map
              [:street :string]
              [:city :string]
              [:zip :int]
              [:lonlat [:tuple :double :double]]]]],
           :id 0,
           :path [],
           :in [],
           :value
           {:id "a",
            :tags #{:b},
            :address
            {:street "somewhere", :city "a city", :zip 234, :lonlat [1.0 2.0]}}}
          {:schema :string,
           :id 1,
           :path [[0 :id]],
           :in [:id],
           :leaf true,
           :value "a"}
          {:schema [:set :keyword],
           :id 2,
           :path [[1 :tags]],
           :in [:tags],
           :value #{:b}}
          {:schema :keyword,
           :id 3,
           :path [[1 :tags] 0],
           :in [:tags 0],
           :leaf true,
           :value :b}
          {:schema
           [:map
            [:street :string]
            [:city :string]
            [:zip :int]
            [:lonlat [:tuple :double :double]]],
           :id 4,
           :path [[2 :address]],
           :in [:address],
           :value
           {:street "somewhere", :city "a city", :zip 234, :lonlat [1.0 2.0]}}
          {:schema :string,
           :id 1,
           :path [[2 :address] [0 :street]],
           :in [:address :street],
           :leaf true,
           :value "somewhere"}
          {:schema :string,
           :id 1,
           :path [[2 :address] [1 :city]],
           :in [:address :city],
           :leaf true,
           :value "a city"}
          {:schema :int,
           :id 5,
           :path [[2 :address] [2 :zip]],
           :in [:address :zip],
           :leaf true,
           :value 234}
          {:schema [:tuple :double :double],
           :id 6,
           :path [[2 :address] [3 :lonlat]],
           :in [:address :lonlat],
           :value [1.0 2.0]}
          {:schema :double,
           :id 7,
           :path [[2 :address] [3 :lonlat] 0],
           :in [:address :lonlat 0],
           :leaf true,
           :value 1.0}
          {:schema :double,
           :id 7,
           :path [[2 :address] [3 :lonlat] 1],
           :in [:address :lonlat 1],
           :leaf true,
           :value 2.0}]
         (explode Address
                  {:id "a"
                   :tags #{:b}
                   :address {:street "somewhere"
                             :city "a city"
                             :zip 234
                             :lonlat [1.0 2.0]}})))
  (is (= '[[0 [let [a 1] [inc 42]]]
           [1 [let [a 1] [inc 42]]]
           [2 let]
           [3 [a 1]]
           [4 a]
           [0 1]
           [5 1]
           [6 1]
           [7 1]
           [0 [inc 42]]
           [8 [inc 42]]
           [0 inc]
           [4 inc]
           [0 42]
           [5 42]
           [6 42]
           [7 42]]
         (mapv (juxt :id :value) (explode Expr '[let [a 1] [inc 42]]))))
  )

(defn substitutable-vals [schema v]
  (-> (group-by :id (explode schema v))
      (update-vals #(mapv :value %))))

(deftest substitutable-vals-test
  (is (= {0 [[[[[nil]]]] [[[nil]]] [[nil]] [nil] nil],
          1 [[[[[nil]]]] [[[nil]]] [[nil]] [nil] nil]}
         (substitutable-vals [:schema {:registry {"Cons" [:maybe [:sequential [:ref "Cons"]]]}} "Cons"]
                             [[[[nil]]]])))
  (is (= '{0 [[let [a 1] [let [a 1] a]] 1 [let [a 1] a] 1 a],
           1 [[let [a 1] [let [a 1] a]] [let [a 1] a]],
           2 [let let],
           3 [[a 1] [a 1]],
           4 [a a a],
           5 [1 1],
           6 [1 1],
           7 [1 1]}
         (substitutable-vals Expr ['let ['a 1] ['let ['a 1] 'a]])))
  )

(defn schema-at-key [schema v k]
  (into []
        (keep (fn [{:keys [schema value in]}]
                (let [last-in (peek in)
                      last-in (cond-> last-in
                                (vector? last-in) peek)]
                  (when (= last-in k)
                    schema))))
        (explode schema v)))

(deftest schema-at-key-test
  (is (= [[:tuple :double :double]]
         (schema-at-key Address
                        {:id "a"
                         :tags #{:b}
                         :address {:street "somewhere"
                                   :city "a city"
                                   :zip 234
                                   :lonlat [1.0 2.0]}}
                        :lonlat))))

(defn schema-at-val [schema v leaf-val]
  (into []
        (keep (fn [{:keys [schema value]}]
                (when (= value leaf-val)
                  schema)))
        (explode schema v)))

(deftest schema-at-val-test
  (is (= [:string]
         (schema-at-val Address
                        {:id "a"
                         :tags #{:b}
                         :address {:street "somewhere"
                                   :city "a city"
                                   :zip 234
                                   :lonlat [1.0 2.0]}}
                        "a")))
  (is (= [:double]
         (schema-at-val Address
                        {:id "a"
                         :tags #{:b}
                         :address {:street "somewhere"
                                   :city "a city"
                                   :zip 234
                                   :lonlat [1.0 2.0]}}
                        1.0)))
  (is (= [] ;; weird case, :id is a mandatory :map key, not a schema position
         (schema-at-val Address
                        {:id "a"
                         :tags #{:b}
                         :address {:street "somewhere"
                                   :city "a city"
                                   :zip 234
                                   :lonlat [1.0 2.0]}}
                        :id))))

