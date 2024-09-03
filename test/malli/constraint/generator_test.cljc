(ns malli.constraint.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop' :refer [for-all]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.json-schema-test :as json-schema-test]
            [malli.util :as mu]
            #?(:clj  [malli.test-macros :refer [when-env]]
               :cljs ["@js-joda/timezone/dist/js-joda-timezone-10-year-range"]))
  #?(:cljs (:require-macros [malli.test-macros :refer [when-env]])))

(defn shrink
  ([?schema] (shrink ?schema nil))
  ([?schema {:keys [seed]}]
  (-> (quick-check 1 (for-all [s (mg/generator ?schema)] false) {:seed (or seed 0)})
      :shrunk
      :smallest
      first
      (get 's))))

;; these generators don't work well with :and but do with constraints
;; because the generators are much more specific. they also grow
;; and shrink better because such-that fails a lot less often
;; and the size remains more consistent.
(deftest int-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and int? [:> 739] [:< 741]])))))
  (is (= 740 (mg/generate [:int {:> 739 :< 741}])))
  (is (= 740 (mg/generate [:int {:and [[:not [:<= 739]]
                                       [:not [:>= 741]]]}])))
  (is (= 740 (shrink [:int {:> 739 :< 741}])))
  (dotimes [_ 100]
    (is (every? #{740}
                (mg/sample [:int {:> 739 :< 741}]
                           {:size 1000}))))
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10]
          (doall (mg/sample [:and int? [:> 10] [:< 100]]
                            {:size 1000})))))
  (is (doall (mg/sample [:int {:> 10 :< 100}]
                        {:seed 123})))
  (is (= (mg/sample [:int {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:int {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0})))
  (is (= 11 (shrink [:int {:> 10 :< 100}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:> ##Inf}])))
  #?(:clj (is (thrown-with-msg?
                Exception
                #":malli\.generator/int-generator-min-value-failure"
                (shrink [:int {:> Long/MAX_VALUE}]))))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:min ##Inf :max ##Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-bounds-must-be-ints"
        (shrink [:int {:< ##-Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/int-generator-max-value-failure"
        (shrink [:int {:< Long/MIN_VALUE}])))
  )

(deftest double-constraint-generator-test
  (is (thrown?
        #?(:clj Exception, :cljs js/Error)
        (dotimes [_ 10] (doall (mg/sample [:and :double [:> 739] [:< 741]])))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:> 739.000001 :< 739.000002}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (dotimes [_ 10]
    (let [vs (mg/sample [:double {:and [[:not [:<= 739.000001]]
                                        [:not [:>= 739.000002]]]}]
                        {:size 1000})]
      (is (< 500 (count (distinct vs))))
      (is (every? #(< 739.000001 % 739.000002)
                  vs))))
  (is (= 739.0000010000001
         (shrink [:double {:> 739.000001 :< 739.000002}])
         (shrink [:double {:and [[:not [:<= 739.000001]]
                                 [:not [:>= 739.000002]]]}])))
  (is (= (mg/sample [:double {:> 10 :< 100}]
                    {:size 1000
                     :seed 0})
         (mg/sample [:double {:gen/> 10 :gen/< 100}]
                    {:size 1000
                     :seed 0})))
  (is (= 10.0 (shrink [:double {:>= 10}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/double-generator-min-value-failure"
        (shrink [:double {:> ##Inf}])))
  (is (thrown-with-msg?
        #?(:clj Exception, :cljs js/Error)
        #":malli\.generator/double-generator-max-value-failure"
        (shrink [:double {:< ##-Inf}])))
  #?(:clj (is (= 10.000000000000002 (shrink [:double {:> 10 :< 100}])))
     :cljs (is (= 10.001 (shrink [:double {:> 10 :< 100}]))))
  (is (= 10.00000000001 (shrink [:double {:>= 10.00000000001 :< 100}])))
  (is (= 9.999999999999999 (shrink [:double {:>= 9.999999999999999 :< 100}]))))

(deftest string-constraint-generate-test
  (testing ":and + :min + :max"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint\.solver/min-must-be-non-negative"
          (mg/generate [:string {:min -1}])))
    (is (= ["" "W" "pC" "4C" "x" "61" "8K8" "X5" "I4v" "sy3VC"]
           (vec (mg/sample [:string {}]
                           {:seed 0}))
           (vec (mg/sample [:string {:and []}]
                           {:seed 0}))))
    (is (= ["Q0o7BnE37b" "6zNfuEdSsmp" "pwBdA45T9xxH" "4t1X2NXEI963" "p6Xp7IS2qOG" "6h1299fiSw7l" "8K9e51XMppRzg" "X4W88PP18l0P" "I4r432WZE70lJ" "sy3V813e055M00E"]
           (vec (mg/sample [:string {:min 10}]
                           {:seed 0}))
           (vec (mg/sample [:string {:and [[:min 10]]}]
                           {:seed 0}))))
    (is (= ["C" "6zNfN" "pwBdA45T9C" "4t1X2Nl" "p6XC" "6ho" "8K99" "X40" "I4v" "sy3VC"]
           (vec (mg/sample [:string {:max 10}]
                           {:seed 0}))
           (vec (mg/sample [:string {:and [[:max 10]]}]
                           {:seed 0}))))
    (is (= ["Q0o2" "6zNfN" "pwBdA9" "4t1X2C" "p6XpD" "6h12u" "8K9ew" "X4W9" "I4r47" "sy3VC"]
           (vec (mg/sample [:string {:min 4 :max 6}]
                           {:seed 0}))
           (vec (mg/sample [:string {:and [[:min 4] [:max 6]]}]
                           {:seed 0}))))
    (is (every? seq (mg/sample [:string {:min 1}])))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:min 10 :max 9}])))))
