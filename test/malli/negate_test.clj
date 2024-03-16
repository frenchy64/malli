(ns malli.negate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.negate :as mn]
            [malli.error :as me]))

(defn -negs [{:keys [pass fail schema negated]}]
  (let [x (gensym 'x)]
    (doseq [pass pass]
      (is (m/explain [:not schema] pass))
      (is (nil? (m/explain schema pass)))
      (is (m/explain negated pass))
      (is (nil? (m/explain [:not negated] pass))))
    (doseq [fail fail]
      (is (nil? (m/explain [:not schema] fail)))
      (is (m/explain schema fail))
      (is (nil? (m/explain negated fail)))
      (is (m/explain [:not negated] fail)))
    (is (= negated (m/form (mn/negate schema))))))

(defn negs [m]
  (-negs (dissoc m :double-negated))
  (when-not (:no-double-negation m)
    (-negs (-> m
               (set/rename-keys {:schema :negated
                                 :negated :schema
                                 :pass :fail
                                 :fail :pass})
               (update :negated #(:double-negated m %))))))

(defn -contains? [& xs]
  `(fn [~'x]
     (or ~@(map (fn [x]
                  `(contains? ~'x '~x))
                xs))))

(def -not-map? `(fn [~'x] (not (map? ~'x))))
(def -not-map-spec [:fn {:error/message {:en "should not be a map"}} -not-map?])

(deftest negate-test
  (testing ":="
    (negs {:schema [:not [:= 1]]
           :pass [nil 2 "a"]
           :fail [1]
           :negated [:= 1]
           :double-negated [:not= 1]})
    (negs {:schema [:= 1]
           :pass [1]
           :fail [nil 2 "a"]
           :negated [:not= 1]}))
  (testing ":or + :and"
    (negs {:schema [:or [:= 1] [:= 2]]
           :pass [1 2]
           :fail [nil 3]
           :negated [:and [:not= 1] [:not= 2]]})
    (negs {:schema [:or [:= 1] [:= 2] [:= 3]]
           :pass [1 2 3]
           :fail [nil 4]
           :negated [:and [:not= 1] [:not= 2] [:not= 3]]}))
  (testing ":map"
    (negs {:schema [:map]
           :pass [{} {:a 1}]
           :fail [1 nil]
           :negated [:not #'clojure.core/map?]
           :no-double-negation true})
    (negs {:schema [:map [:a [:= 1]]]
           :pass [{:a 1} {:a 1 :extra 42}]
           :fail [{} {:a 2} 1 nil]
           :negated [:or
                     [:not #'clojure.core/map?]
                     ;; missing :a
                     [:map [:a {:optional true} :never]]
                     ;; bad :a
                     [:map [:a [:not= 1]]]]
           :no-double-negation true})
    (negs {:schema [:map
                    [:a [:= 1]]
                    [:b [:= 2]]]
           :pass [{:a 1 :b 2}]
           :fail [{} {:a 1} {:a 2} {:a 2 :b 1} 1 nil]
           :negated [:or
                     [:not #'clojure.core/map?]
                     ;; missing :a
                     [:map [:a {:optional true} :never]]
                     ;; missing :b
                     [:map [:b {:optional true} :never]]
                     ;; bad :a
                     [:map [:a [:not= 1]]]
                     ;; bad :b
                     [:map [:b [:not= 2]]]
                     ]
           :no-double-negation true})
    (negs {:schema [:map [:a {:optional true} [:= 1]]]
           :pass [{:a 1} {} {:a 1} {:b 2}]
           :fail [{:a 2} {:a 2 :b 1} 1 nil]
           :negated [:or
                     [:not #'clojure.core/map?]
                     ;; bad :a
                     [:map [:a [:not= 1]]]]
           :no-double-negation true})
    (negs {:schema [:map
                    [:a {:optional true} [:= 1]]
                    [:b [:= 2]]]
           :pass [{:a 1 :b 2} {:b 2}]
           :fail [{:a 2} {:b 1} {} 1 nil]
           :negated [:or
                     ;; non map
                     [:not #'clojure.core/map?]
                     ;; missing :b
                     [:map [:b {:optional true} :never]]
                     ;; bad :a
                     [:map [:a [:not= 1]]]
                     ;; bad :b
                     [:map [:b [:not= 2]]]]
           :no-double-negation true}))
  (testing ":map-of"
    (negs {:schema [:map-of [:= 1] [:= 2]]
           :pass [{} {1 2}]
           :fail [{:a "foo"} {1 1} {2 2} {2 1} nil 1 :a
                  {1 1 2 2}]
           :negated [:or
                     [:not #'clojure.core/map?]
                     [:into-map
                      [:cat
                       [:* [:tuple :any :any]]
                       [:+ [:tuple [:not= 1] :any]]
                       [:* [:tuple :any :any]]]]
                     [:into-map
                      [:cat
                       [:* [:tuple :any :any]]
                       [:+ [:tuple :any [:not= 2]]]
                       [:* [:tuple :any :any]]]]]
           :no-double-negation true}))
  (testing ":vector"
    (negs {:schema [:vector [:= 1]]
           :pass [[] [1] [1 1] [1 1 1]]
           :fail [[:a] nil 1 :a
                  [1 :b]
                  [2 3 1 5]
                  [1 2 1]
                  [2 1 2]
                  [2 1 1 1 1 2]
                  [1 2 1 2 1 2 1]]
           :negated [:or
                     [:not #'vector?]
                     [:and [:cat {:gen/fmap #'vec}
                            [:* :any]
                            [:+ [:not= 1]]
                            [:* :any]]
                      #'vector?]]
           :no-double-negation true}))
  (testing ":nil + :some"
    (negs {:schema :nil
           :pass [nil]
           :fail [{} 1 :a "asdf"]
           :negated :some}))
  (testing ":any + :never"
    (negs {:schema :any
           :pass [{} 1 :a "asdf"]
           :fail []
           :negated :never}))
  (testing ":string"
    (negs {:schema :string
           :pass ["asdf" ""]
           :fail [{} 1 :a]
           :negated [:not :string]
           :no-double-negation true})
    (negs {:schema [:string {:min 0}]
           :pass ["asdf" ""]
           :fail [{} 1 :a]
           :negated [:not :string]
           :no-double-negation true})
    (negs {:schema [:string {:min 1}]
           :pass ["asdf" "b"]
           :fail ["" {} 1 :a]
           :negated [:or
                     [:not :string]
                     [:string {:max 0}]]
           :no-double-negation true})
    (negs {:schema [:string {:max 10}]
           :pass ["asdf" "b" ""]
           :fail ["12345678910" {} 1 :a]
           :negated [:or
                     [:not :string]
                     [:string {:min 11}]]
           :no-double-negation true})
    (negs {:schema [:string {:min 5 :max 10}]
           :pass ["12345" "6543210"]
           :fail ["asdf" "b" "" "12345678910" {} 1 :a]
           :negated [:or
                     [:not :string]
                     [:string {:max 4}]
                     [:string {:min 11}]]
           :no-double-negation true}))
  (testing ":int"
    (negs {:schema :int
           :pass [4 0]
           :fail [{} "a" :a]
           :negated [:not :int]
           :no-double-negation true})
    (negs {:schema [:int {:min 0}]
           :pass [4 0]
           :fail [{} "a" :a]
           :negated [:not :int]
           :no-double-negation true})
    (negs {:schema [:int {:min 1}]
           :pass [4 1]
           :fail [0 {} :a]
           :negated [:or
                     [:not :int]
                     [:int {:max 0}]]
           :no-double-negation true})
    (negs {:schema [:int {:max 10}]
           :pass [4 1 0]
           :fail [11 {} "a" :a]
           :negated [:or
                     [:not :int]
                     [:int {:min 11}]]
           :no-double-negation true})
    (negs {:schema [:int {:min 5 :max 10}]
           :pass [5 7]
           :fail [4 1 0 11 {} "a" :a]
           :negated [:or
                     [:not :int]
                     [:int {:max 4}]
                     [:int {:min 11}]]
           :no-double-negation true}))
  (testing ":boolean"
    (negs {:schema :boolean
           :pass [true false]
           :fail [{} "a" 1 :a]
           :negated [:not :boolean]}))
  (testing ":keyword"
    (negs {:schema :keyword
           :pass [:true :false :a/b]
           :fail [{} "a" 1 true 'a]
           :negated [:not :keyword]}))
  (testing ":symbol"
    (negs {:schema :symbol
           :pass ['foo 'bar 'foo/bar]
           :fail [{} "a" 1 :a true]
           :negated [:not :symbol]}))
  (testing ":qualified-keyword"
    (negs {:schema :qualified-keyword
           :pass [:a/b]
           :fail [:true :false {} "a" 1 true 'a]
           :negated [:not :qualified-keyword]}))
  (testing ":qualified-symbol"
    (negs {:schema :qualified-symbol
           :pass ['foo/bar]
           :fail ['foo 'bar {} "a" 1 :a true]
           :negated [:not :qualified-symbol]}))
  (testing ":uuid"
    (negs {:schema :uuid
           :pass [(random-uuid)]
           :fail ['foo 'bar {} "a" 1 :a true]
           :negated [:not :uuid]}))
  (testing ":maybe"
    (negs {:schema [:maybe :nil]
           :pass [nil]
           :fail [1 :a 'a false "a" {} []]
           :negated :some
           :no-double-negation true})
    (negs {:schema [:maybe :some]
           :pass [nil 1 :a 'a false "a" {} []]
           :fail []
           :negated [:and :nil :some]
           :no-double-negation true})
    (negs {:schema [:maybe :int]
           :pass [nil 1]
           :fail [:a 'a false "a" {} []]
           :negated [:and [:not :int] :some]
           :no-double-negation true}))
  (testing ":tuple"
    (negs {:schema [:tuple]
           :pass [[]]
           :fail [[1] [:a false] :a 'a false "a"]
           :negated [:or
                     [:not #'vector?]
                     [:vector {:min 1} :any]]
           :no-double-negation true})
    (negs {:schema [:tuple :int]
           :pass [[1]]
           :fail [[] [:a] [:a false] :a 'a false "a"]
           :negated [:or
                     [:not #'vector?]
                     [:tuple [:not :int]]
                     [:vector {:max 0} :any]
                     [:vector {:min 2} :any]]
           :no-double-negation true})
    (negs {:schema [:tuple [:= 1] [:= 2]]
           :pass [[1 2]]
           :fail [[] [:a] [1 1] [2 2] [2 1] [:a false] [4 1 2 5 4] :a 'a false "a"]
           :negated [:or
                     [:not #'vector?]
                     [:tuple [:not= 1] :any]
                     [:tuple :any [:not= 2]]
                     [:vector {:max 1} :any]
                     [:vector {:min 3} :any]]
           :no-double-negation true}))
  (testing ":ref"
    (negs {:schema [:schema
                    {:registry {::simple :int}}
                    ::simple]
           :pass [1 2]
           :fail [nil :a]
           :negated [:not :int]
           :no-double-negation true})
    (negs {:schema [:schema
                    {:registry {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                                ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                    ::ping]
           :pass [nil
                  ["ping" ["pong" nil]]
                  ["ping" ["pong" ["ping" ["pong" nil]]]]]
           :fail [1
                  ["ping"]
                  ["ping" ["ping" nil]]
                  ["pong" ["ping" nil]]
                  ["ping" ["pong" ["ping" ["pong" "ping"]]]]]
           :negated
           [:and
            [:or
             [:not #'clojure.core/vector?]
             [:tuple [:not= "ping"] :any]
             [:tuple
              :any
              [:schema {:registry {:malli.negate-test/pong
                                   [:and
                                    [:or
                                     [:not #'clojure.core/vector?]
                                     [:tuple [:not= "pong"] :any]
                                     [:tuple :any [:and
                                                   [:or
                                                    [:not #'clojure.core/vector?]
                                                    [:tuple [:not= "ping"] :any]
                                                    [:tuple :any [:ref :malli.negate-test/pong]]
                                                    [:vector {:max 1} :any]
                                                    [:vector {:min 3} :any]]
                                                   :some]]
                                     [:vector {:max 1} :any]
                                     [:vector {:min 3} :any]]
                                    :some]}}
               [:ref :malli.negate-test/pong]]]
             [:vector {:max 1} :any]
             [:vector {:min 3} :any]]
            :some]
           #_
           [:schema {:registry {::ping [:and
                                        [:or
                                         [:not #'vector?]
                                         [:tuple [:not= "ping"] :any]
                                         [:tuple :any [:and
                                                       [:or
                                                        [:not #'vector?]
                                                        [:tuple [:not= "pong"] :any]
                                                        [:tuple :any [:ref ::ping]]
                                                        [:vector {:max 1} :any]
                                                        [:vector {:min 3} :any]]
                                                       :some]]
                                         [:vector {:max 1} :any]
                                         [:vector {:min 3} :any]]
                                        :some]}}
            ::ping]
           #_
           [:schema
                     {:registry {::ping [:and
                                         [:or
                                          [:not #'vector?]
                                          [:tuple [:not= "ping"] :any]
                                          [:tuple :any [:ref ::pong]]
                                          [:vector {:max 1} :any]
                                          [:vector {:min 3} :any]]
                                         :some]
                                 ::pong [:and
                                         [:or
                                          [:not #'vector?]
                                          [:tuple [:not= "pong"] :any]
                                          [:tuple :any [:ref ::ping]]
                                          [:vector {:max 1} :any]
                                          [:vector {:min 3} :any]]
                                         :some]}}
                     ::ping]
           :no-double-negation true})))

(comment
  (m/validate [:map] {})
  (m/validate [:not [:map]] {})
  (m/validate [:not [:map]] {:a 1})
  (m/validate [:not [:map]] nil)

  (m/validate [:map [:a [:= 1]]] {:a 1})
  (m/validate [:map [:a [:= 1]]] {:a 2})
  (m/validate [:not [:map [:a [:= 1]]]] {:a 1})
  (m/validate [:not [:map [:a [:= 1]]]] {:a 2})
  (m/validate
    [:multi {:dispatch #'clojure.core/map?}
     [true [:multi {:dispatch '(clojure.core/fn [x] (clojure.core/or (clojure.core/contains? x (quote :a))))}
            [true [:map [:a [:not= 1]]]]
            [false [:map-of [:enum :a] :any]]]]
     [false :any]]
    {})
  (require '[malli.generator :as mg])
  (filter map?
        (mg/sample [:multi {:dispatch #'clojure.core/map?}
                    [true [:multi {:dispatch '(clojure.core/fn [x] (clojure.core/or (clojure.core/contains? x (quote :a))
                                                                                    (clojure.core/contains? x (quote :b))))}
                           [true [:map
                                  [:a {:optional true} [:not= 1]]
                                  [:b {:optional true} [:not= 1]]]]
                           [false :nil]]]
                    [false :nil]]
                   {:size 10}))
  (m/validate [:multi {:dispatch #'clojure.core/map?}
              [true [:multi {:dispatch '(clojure.core/fn [x] (clojure.core/or (clojure.core/contains? x (quote :a))
                                                                              (clojure.core/contains? x (quote :b))))}
                     [true [:map
                            [:a {:optional true} [:not= 1]]
                            [:b {:optional true} [:not= 1]]]]
                     [false :any]]]
              [false :any]]
              1)
  (m/validate [:tuple] [])
  (m/validate [:tuple] [])
  (m/validate
    [:cat
     [:* [:= 1]]
     [:+ [:not= 1]]
     [:* [:= 1]]]
    [2 3 1 5])
  (m/explain
    [:cat
     [:* [:= 1]]
     [:+ [:not= 1]]
     [:* [:= 1]]]
    [2 3 1 5])
  (mg/generate
    [:or
     ;[:not #'vector?]
     [:and [:cat {:gen/fmap #'vec}
            [:alt
             [:cat
              [:+ :any]
              [:* [:not= 1]]
              [:* :any]]
             [:cat
              [:* :any]
              [:* [:not= 1]]
              [:+ :any]]]]
      #'vector?]]
    {:size 1})

  (m/explain
    [:or
     [:not #'vector?]
     [:and [:cat {:gen/fmap #'vec}
            [:* :any]
            [:+ [:not= 1]]
            [:* :any]]
      #'vector?]]
    [1 2 1 2 1])
  )


