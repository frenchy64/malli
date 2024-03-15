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
           :negated -not-map-spec
           :no-double-negation true})
    (negs {:schema [:map [:a [:= 1]]]
           :pass [{:a 1} {:a 1 :extra 42}]
           :fail [{} {:a 2} 1 nil]
           :negated [:multi {:dispatch #'clojure.core/map?}
                     [true [:multi {:dispatch '(clojure.core/fn [x]
                                                 (clojure.core/cond
                                                   (clojure.core/contains? x (quote :a)) (quote :a)
                                                   :else ::mn/default))}
                            [:a [:map [:a [:not= 1]]]]
                            [::mn/default [:map [:a {:optional true} :never]]]]]
                     [false [:not #'clojure.core/map?]]]
           :no-double-negation true})
    (negs {:schema [:map
                    [:a [:= 1]]
                    [:b [:= 2]]]
           :pass [{:a 1 :b 2}]
           :fail [{} {:a 1} {:a 2} {:a 2 :b 1} 1 nil]
           :negated [:multi {:dispatch #'clojure.core/map?}
                     [true [:multi {:dispatch '(clojure.core/fn [x]
                                                 (clojure.core/cond
                                                   (clojure.core/contains? x (quote :a)) (quote :a)
                                                   (clojure.core/contains? x (quote :b)) (quote :b)
                                                   :else ::mn/default))}
                            [:a [:map [:a [:not= 1]] [:b {:optional true} [:not= 2]]]]
                            [:b [:map [:a {:optional true} [:not= 1]] [:b [:not= 2]]]]
                            [::mn/default [:map [:a {:optional true} :never] [:b {:optional true} :never]]]]]
                     [false [:not #'clojure.core/map?]]]
           :no-double-negation true})
    (negs {:schema [:map [:a {:optional true} [:= 1]]]
           :pass [{:a 1} {}]
           :fail [{:a 2} 1 nil]
           :negated [:multi {:dispatch #'clojure.core/map?}
                     [true [:multi {:dispatch '(clojure.core/fn [x] (clojure.core/or (clojure.core/contains? x (quote :a))))}
                            [true [:map [:a {:optional true} [:not= 1]]]]
                            [false [:map-of [:enum :a] :any]]]]
                     [false :any]]
           :no-double-negation true}))
  )

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
  )

