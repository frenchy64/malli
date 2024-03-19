(ns malli.check-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn is-all-good [schema vs]
  (testing "good"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (is (nil? (mg/check schema f)))))))

(defn is-all-bad [schema vs]
  (testing "bad"
    (doseq [[i f] (map-indexed vector vs)]
      (testing i
        (is (mg/check schema f {::mg/all-iterations 1000}))))))

(def good-identities [identity
                      (fn [a] a)
                      (fn [a] (identity a))])
(def bad-identities [(fn [_] nil)
                     (fn [a] (when (uuid? a) a))])

(def identity-spec (m/all [a] [:=> [:cat a] a]))

(deftest identity-test
  (is-all-good identity-spec good-identities)
  (is-all-bad identity-spec bad-identities))

(def good-takes [take])
(def bad-takes [(constantly [10])])

(def take-spec (m/all [a] [:=> [:cat #'nat-int? [:sequential a]] [:sequential a]]))

(deftest take-test
  (is-all-good take-spec good-takes)
  (is-all-bad take-spec bad-takes))

(def good-maps [map
                (fn [f c]
                  (map (comp identity f) c))
                (fn [f c]
                  (map (comp f identity) c))
                (comp #(map identity %) map)
                (constantly ())
                (comp #(filter nil? %) map)])
(def bad-maps [(constantly [1])
               #_#_#_;;FIXME won't shrink if function arg used incorrectly
               (comp #(map str %) map)
               (fn [f c]
                 (map (comp str f) c))
               (fn [f c]
                 (map (comp f str) c))])

(def map-spec (m/all [a b] [:=> [:cat [:=> [:cat a] b] [:sequential a]] [:sequential b]]))

(deftest map-test
  (is-all-good map-spec good-maps)
  (is-all-bad map-spec bad-maps))

(def memoize-spec (m/all [a :.. b]
                         [:=>
                          [:cat [:=> [:cat [:.. a]] b]]
                          [:=> [:cat [:.. a]] b]]))

(comment
  (inst memoize-spec [:* :int] :int)
  (inst memoize-spec :int :int)
  (inst memoize-spec [:+ :int] :int)
  [:=>
   [:cat [:=> [:cat [:cat :int :bool]] b]]
   [:=> [:cat [:cat :int :bool]] b]]
  [:=>
   [:cat [:=> [:cat [:* :int]] b]]
   [:=> [:cat [:* :int]] b]]
  [:=>
   [:cat [:=> [:cat [:+ :int]] b]]
   [:=> [:cat [:+ :int]] b]]

  [:all [:.. :Schema]
   `(fn [a# b#]
      [:=>
       [:cat [:=> [:cat [:.. a]] b]]
       [:=> [:cat [:.. a]] b]])]

  (let [a 'a b 'b]
    `[:all [~a :.. ~b]
      [:=>
       [:cat [:=> [:cat [:.. ~a]] ~b]]
       [:=> [:cat [:.. ~a]] ~b]
       :fn '(fn [~'x] ~a)]])

  (m/all [a :.. b]
         [:=>
          [:cat [:=> [:cat [:.. a]] b]]
          [:=> [:cat [:.. a]] b]
          :fn '(fn [x] ~a)])

  (m/all [a :.. b]
         [:=>
          [:cat [:=> [:cat [:.. a]] b]]
          [:=> [:cat [:.. a]] b]
          :fn `(fn [x] ~a)])

  (m/all2 [a b]
          [:=>
           [:cat [:=> [:cat [:.. a]] b]]
           [:=> [:cat [:.. a]] b]
           :fn `(fn [~'x] ~a)])

  '[:all [a :.. b]
    (fn [a b]
      [:=>
       [:cat [:=> [:cat [:.. a]] b]]
       [:=> [:cat [:.. a]] b]
       :fn `(fn [x] ~a)])]
  [:=>
   [:cat [:=> [:cat [:.. ?A]] ?B]]
   [:=> [:cat [:.. ?A]] ?B]
   :fn `(fn [x] ~?A)]

  ;; reducer
  (m/tfn [a b]
         [:=>
          [:cat b a]
          [:or b [#_:reduced :tuple a]]])
  [:schema {:registry {::Reducer (m/tfn [a b]
                                        [:function
                                         [:=> [:cat [:? b]] b]
                                         [:=> [:cat b a] [:or b #_[#_:reduced :tuple a]]]])
                       ::Transducer (m/tfn [in out]
                                           (m/all [r]
                                                  [:=> [:cat [:tapp [:ref ::Reducer] out r]]
                                                   [:cat [:tapp [:ref ::Reducer] in r]]]))
                       `map (m/all2 [a #_:+ b c]
                                    [:function
                                     [:=> [:cat [:=> [:cat b] c]] [:ref ::Transducer b c]]
                                     [:=> [:cat
                                           [:=> [:cat a] b]
                                           [:.. [:sequential a] a]]
                                      [:sequential b]]])}}
   [:ref `map]]
  )
