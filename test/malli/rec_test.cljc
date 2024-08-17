(ns malli.rec-test
  (:require [clojure.string :as str]
            [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.edn :as edn]
            [malli.generator :as mg]
            [malli.error :as me]
            [malli.impl.util :as miu]
            [malli.registry :as mr]
            [malli.transform :as mt]
            [malli.util :as mu]
            [malli.rec :as rec]
            #?(:clj [malli.test-macros :refer [when-env]]))
  #?(:clj  (:import (clojure.lang IFn PersistentArrayMap PersistentHashMap))
     :cljs (:require-macros [malli.test-macros :refer [when-env]])))

(def options {:registry (mr/composite-registry m/default-registry (rec/schemas) (mu/schemas))})

(deftest rec-test
  (is (= [:rec [:onion] [:seqable :onion]]
         (m/form [:rec [:onion] [:seqable :onion]] options)))
  (is (= [:seqable [:rec [:onion] [:seqable :onion]]]
         (m/form (m/deref [:rec [:onion] [:seqable :onion]] options))))
  (is (= [:rec [:x] [:=> [:cat :x] :x]]
         (m/form [:rec [:x] [:=> [:cat :x] :x]] options)))
  (is (= [:rec [:x] [:-> :x :x]]
         (m/form [:rec [:x] [:-> :x :x]] options)))
  (is (= [:rec [:x] [:=> [:cat [:rec [:y] :y]] :x]]
         (m/form [:rec [:x] [:=> [:cat [:rec [:y] :y]] :x]] options)))
  (is (= [:rec [:x] [:-> [:rec [:x] :x] :x]]
         (m/form [:rec [:x] [:-> [:rec [:x] :x] :x]] options)))
  (is (= [:=> [:cat [:rec [:x] [:=> [:cat :x] :x]]] [:rec [:x] [:=> [:cat :x] :x]]]
         (m/form (m/deref [:rec [:x] [:=> [:cat :x] :x]] options))))
  (is (= [:->
          [:rec [:x] [:-> :x :x]]
          [:rec [:x] [:-> :x :x]]]
         (m/form (m/deref [:rec [:x] [:-> :x :x]]
                          options))))
  (is (= [:rec [:y] :y]
         (m/form [:rec [:y] :y] options)))
  (is (= [:rec [:y] [:rec [:y] :y]]
         (m/form [:rec [:y] [:rec [:y] :y]] options)))
  (is (= [:rec [:x] [:rec [:y] :x]]
         (m/form [:rec [:x] [:rec [:y] :x]]
                 options)))
  (is (= :int
         (m/form (m/deref [:rec [:x] :int]
                          options))))
  (is (= [:rec [:y] [:rec [:x] [:rec [:y] :x]]]
         (m/form (m/deref [:rec [:x] [:rec [:y] :x]]
                          options))))
  (is (= [:rec [:x] :x]
         (m/form (m/deref [:rec [:x] [:rec [:x] :x]]
                          options))))
  (is (= [:=> [:cat [:rec [:a] [:=> [:cat :a] :a]]] [:rec [:a] [:=> [:cat :a] :a]]]
         (m/form (m/deref [:rec [:a] [:=> [:cat :a] :a]] options))))
  (is (= [:-> [:rec [:a] [:-> :a :a]] [:rec [:a] [:-> :a :a]]]
         (m/form (m/deref [:rec [:a] [:-> :a :a]] options))))
  (is (= [:merge [:rec [:M] [:merge :M [:map [:x :M]]]] [:map [:x [:rec [:M] [:merge :M [:map [:x :M]]]]]]]
         (-> [:rec [:M]
              [:merge :M [:map [:x :M]]]]
             (m/schema options)
             m/deref
             m/form)))
  (is (= [:-> [:rec [:M] [:-> :M [:merge :M]]]
          [:merge [:rec [:M]
                   [:-> :M [:merge :M]]]]]
         (-> [:rec [:M]
              [:-> :M [:merge :M]]]
             (m/schema options)
             m/deref
             m/form))))

(def ping-pong-rec-schema
  (m/schema
    [:rec [:ping]
     [:maybe [:tuple [:= "ping"]
              [:maybe [:tuple [:= "pong"]
                       :ping]]]]]
    options))

(def ping-pong-rec-validator (m/validator ping-pong-rec-schema))

(deftest rec-validator-test
  (is (ping-pong-rec-validator nil))
  (is (ping-pong-rec-validator ["ping" nil]))
  (is (not (ping-pong-rec-validator ["ping"]))))

(deftest walk-test
  (is (= ::TODO
         (m/walk [:rec [:ping]
                  [:maybe [:tuple [:= "ping"]
                           [:maybe [:tuple [:= "pong"]
                                    :ping]]]]]
                 (fn [s p c o]
                   (prn (m/type s))
                   s)
                 options))))

#_
(deftest f-in-registry-test
  (is (= [:-> [:schema {:registry {::a :any}} :any] :any]
         (m/form (m/deref [:all [:a] [:-> [:schema {:registry {::a :a}} :a] :a]]
                          options)))))

#_
(deftest poly-generator-test
  ;;TODO :P
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli.generator/no-generator"
        (mg/generate [:all [:X] [:=> [:cat :X] :X]] options)))
  ;;via deref
  (is (= {} ((mg/generate (m/deref [:all [:X] [:=> [:cat :X] :X]] options) {:seed 1 :size 2}) 1))))

(defn is-all-good [schema vs]
  (testing "good"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (is (nil? (mg/check schema f options)))))))

(defn is-all-bad  [schema vs]
  (testing "bad"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (try (let [res (mg/check schema f (assoc options ::mg/all-iterations 1000))]
               (is res))
             (catch #?(:clj Exception, :cljs js/Error) e
               (is (= ::m/invalid-input (:type (ex-data e))))))))))

(def good-identities [identity
                      (fn [a] a)
                      (fn [a] (identity a))])
(def bad-identities [(fn [_] nil)
                     (fn [a] (when (uuid? a) a))])

(def identity-specs [[:all [:a] [:=> [:cat :a] :a]]
                     [:all [:a] [:-> :a :a]]])

#_
(deftest identity-test
  (doseq [identity-spec identity-specs]
    (testing (pr-str identity-spec)
      (is-all-good identity-spec good-identities)
      (is-all-bad identity-spec bad-identities))))

(def good-maps [map
                (fn [f c] (map f c))
                (fn [f c] (mapv f c))])
(def bad-maps [(comp #(map str %) map)
               (fn [f c] (map (comp f str) c))
               (fn [f c] (map (comp str f) c))])

(def map-specs [[:all [:a :b] [:=> [:cat [:=> [:cat :a] :b] [:sequential :a]] [:sequential :b]]]
                [:all [:a :b] [:-> [:-> :a :b] [:sequential :a] [:sequential :b]]]])

;; TODO catch higher-order failures and shrink them.
#_
(deftest map-test
  (doseq [map-spec map-specs]
    (testing (pr-str map-spec)
      (is-all-good map-spec good-maps)
      (is-all-bad map-spec bad-maps))))

#_
(deftest all-smart-constructor-destructor-test
  (is (= [:all [:x] [:-> :x :x]]
         (m/form [:all [:x] [:-> :x :x]] options)))
  (is (= [:all [:x :y] [:-> :x :y]]
         (m/form [:all [:x :y] [:-> :x :y]] options)))
  (is (= [:-> :int :boolean]
         (m/form
           (poly/inst (m/schema [:all [:x :y] [:-> :x :y]] options)
                      [:int :boolean]
                      options))))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #"regex-not-kind-schema"
        (poly/inst (m/schema [:all [:x :y] [:-> :x :y]] options)
                   [[:? :int] [:* :boolean]]
                   options))))

(comment
  (m/form
    (m/schema
      [:schema {:registry {::Reducer (m/tfn [a b] [:=> b a b])
                           ::Transducer (m/tfn [in out]
                                               (m/all [r]
                                                      [:=> [::Reducer out r] [::Reducer in r]]))}}
       (m/all [in out] [:=> [:=> in out] [::Transducer in out]])]))
  )
