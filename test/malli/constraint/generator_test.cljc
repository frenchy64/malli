(ns malli.constraint.generator-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
            [malli.generator :as mg]
            [malli.util :as mu]))

(defn add-constraints [options]
  (-> options
      (assoc ::m/constraint-options (mc/base-constraint-extensions))
      (update :registry #(merge (or % (m/default-schemas)) (mc/base-constraints)))))

(deftest string-constraint-generate-test
  (testing ":and + :min + :max"
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.constraint/count-constraint-min"
          (mg/generate [:string {:min -1}] (add-constraints {}))))
    (is (= ["" "W" "pC" "4C" "x" "61" "8K8" "X5" "I4v" "sy3VC"]
           (vec (mg/sample [:string {}]
                           (add-constraints {:seed 0})))
           (vec (mg/sample [:string {:and []}]
                           (add-constraints {:seed 0})))))
    (is (= ["Q0o7BnE37b" "6zNfuEdSsmp" "pwBdA45T9xxH" "4t1X2NXEI963" "p6Xp7IS2qOG" "6h1299fiSw7l" "8K9e51XMppRzg" "X4W88PP18l0P" "I4r432WZE70lJ" "sy3V813e055M00E"]
           (vec (mg/sample [:string {:min 10}]
                           (add-constraints {:seed 0})))
           (vec (mg/sample [:string {:and [[:min 10]]}]
                           (add-constraints {:seed 0})))))
    (is (= ["C" "6zNfN" "pwBdA45T9C" "4t1X2Nl" "p6XC" "6ho" "8K99" "X40" "I4v" "sy3VC"]
           (vec (mg/sample [:string {:max 10}]
                           (add-constraints {:seed 0})))
           (vec (mg/sample [:string {:and [[:max 10]]}]
                           (add-constraints {:seed 0})))))
    (is (= ["Q0o2" "6zNfN" "pwBdA9" "4t1X2C" "p6XpD" "6h12u" "8K9ew" "X4W9" "I4r47" "sy3VC"]
           (vec (mg/sample [:string {:min 4 :max 6}]
                           (add-constraints {:seed 0})))
           (vec (mg/sample [:string {:and [[:min 4] [:max 6]]}]
                           (add-constraints {:seed 0})))))
    (is (every? seq (mg/sample [:string {:min 1}] (add-constraints {}))))
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:min 10 :max 9}] (add-constraints {}))))))

(deftest integer-constraint-generate-test
  (testing ":and + :min + :max"
    (is (= [0 -1 0 -3 0 1 16 0 7 3]
           (mg/sample [:int {}] {:seed 0})
           (mg/sample [:int {}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and []}] (add-constraints {:seed 0}))))
    (is (= [10 11 10 13 10 11 26 10 17 13]
           (mg/sample [:int {:min 10}] {:seed 0})
           (mg/sample [:int {:min 10}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min 10] [:min 5]]}] (add-constraints {:seed 0}))))
    (is (= [0 -1 0 -3 0 1 -16 0 7 3]
           (mg/sample [:int {:max 10}] {:seed 0})
           (mg/sample [:int {:max 10}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:max 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:max 15] [:max 10]]}] (add-constraints {:seed 0}))))
    (is (= [4 5 4 5 4 5 6 4 5 5]
           (mg/sample [:int {:min 4 :max 6}] {:seed 0})
           (mg/sample [:int {:min 4 :max 6}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min 4] [:max 6]]}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min 4] [:max 6] [:min 3] [:max 7]]}] (add-constraints {:seed 0}))))
    (is (= [-5 -6 -5 -8 -5 -6 -9 -5 -8 -8]
           (mg/sample [:int {:min -10 :max -5}] {:seed 0})
           (mg/sample [:int {:min -10 :max -5}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min -10] [:max -5]]}] (add-constraints {:seed 0}))
           (mg/sample [:int {:and [[:min -10] [:max -5] [:min -11] [:max -4]]}] (add-constraints {:seed 0}))))
    (is (every? pos-int? (mg/sample [:int {:min 1}] {})))
    (is (every? pos-int? (mg/sample [:int {:min 1}] (add-constraints {}))))
    (is (every? neg-int? (mg/sample [:int {:max -1}] {})))
    (is (every? neg-int? (mg/sample [:int {:max -1}] (add-constraints {}))))
    #?(:clj (testing "without constraints properties are checked for satisfiability"
              (is (thrown-with-msg?
                    AssertionError,
                    #"(<= min max)"
                    (mg/generate [:int {:min 10 :max 9}] {})))))
    (testing "with constraints the solver signals unsatisfiability with zero solutions"
      (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-int-constraint"
          (mg/generate [:int {:min 10 :max 9}] (add-constraints {})))))))

(deftest double-constraint-generate-test
  (testing ":and + :min + :max"
    (is (= [-1.0 2.0 -0.0 1.0 -1.0 1.0 3.25 -3.0 -0.9375 -2.0]
           (mg/sample [:double {}] {:seed 0})
           (mg/sample [:double {}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and []}] (add-constraints {:seed 0}))))
    (is (= [10.0 16.0 16.0 24.0 16.0 16.0 16.0 10.0 22.0 13.5]
           (mg/sample [:double {:min 10}] {:seed 0})
           (mg/sample [:double {:min 10}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min 10] [:min 5]]}] (add-constraints {:seed 0}))))
    (is (= [0.5 -2.0 -0.0 -3.0 3.0 -0.78125 1.75 1.0 1.10546875 -6.0]
           (mg/sample [:double {:max 10}] {:seed 0})
           (mg/sample [:double {:max 10}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:max 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:max 15] [:max 10]]}] (add-constraints {:seed 0}))))
    (is (= [4.0 4.0 4.0 6.0 4.0 4.0 4.0 4.0 5.5 5.375]
           (mg/sample [:double {:min 4 :max 6}] {:seed 0})
           (mg/sample [:double {:min 4 :max 6}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min 4] [:max 6]]}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min 4] [:max 6] [:min 3] [:max 7]]}] (add-constraints {:seed 0}))))
    (is (= [-7.999999999999998 -8.0 -8.0 -9.0 -8.0 -8.0 -8.0 -7.999999999999998 -9.25 -6.75]
           (mg/sample [:double {:min -10 :max -5}] {:seed 0})
           (mg/sample [:double {:min -10 :max -5}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min -10] [:max -5]]}] (add-constraints {:seed 0}))
           (mg/sample [:double {:and [[:min -10] [:max -5] [:min -11] [:max -4]]}] (add-constraints {:seed 0}))))
    (is (every? pos? (mg/sample [:double {:min 0.0000001}] {})))
    (is (every? pos? (mg/sample [:double {:min 0.0000001}] (add-constraints {}))))
    (is (every? neg? (mg/sample [:double {:max -0.0000001}] {})))
    (is (every? neg? (mg/sample [:double {:max -0.0000001}] (add-constraints {}))))
    #?(:clj (testing "without constraints properties are checked for satisfiability"
              (is (thrown-with-msg?
                    AssertionError
                    (-> "(or (nil? lower-bound) (nil? upper-bound) (<= lower-bound upper-bound))"
                        java.util.regex.Pattern/quote
                        re-pattern)
                    (mg/generate [:double {:min 10 :max 9}] {})))))
    (testing "with constraints the solver signals unsatisfiability with zero solutions"
      (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-double-constraint"
          (mg/generate [:double {:min 10 :max 9}] (add-constraints {})))))))

(deftest float-constraint-generate-test
  (testing ":and + :min + :max"
    (is (= #?(:cljs [-1 2 0 1 -1 1 3.25 -3 -0.9375 -2]
              :default (mapv float [0.5 -2.0 -0.0 -3.0 3.0 -0.78125 1.75 1.0 1.1054688 -6.0]))
           (mg/sample [:float {}] {:seed 0})
           (mg/sample [:float {}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and []}] (add-constraints {:seed 0}))))
    (is (= (mapv float [10.0 16.0 16.0 24.0 16.0 16.0 16.0 10.0 22.0 13.5])
           (mg/sample [:float {:min 10}] {:seed 0})
           (mg/sample [:float {:min 10}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min 10] [:min 5]]}] (add-constraints {:seed 0}))))
    (is (= (mapv float [0.5 -2.0 -0.0 -3.0 3.0 -0.78125 1.75 1.0 1.10546875 -6.0])
           (mg/sample [:float {:max 10}] {:seed 0})
           (mg/sample [:float {:max 10}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:max 10]]}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:max 15] [:max 10]]}] (add-constraints {:seed 0}))))
    (is (= (mapv float [4.0 4.0 4.0 6.0 4.0 4.0 4.0 4.0 5.5 5.375])
           (mg/sample [:float {:min 4 :max 6}] {:seed 0})
           (mg/sample [:float {:min 4 :max 6}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min 4] [:max 6]]}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min 4] [:max 6] [:min 3] [:max 7]]}] (add-constraints {:seed 0}))))
    (is (= (mapv float [-7.999999999999998 -8.0 -8.0 -9.0 -8.0 -8.0 -8.0 -7.999999999999998 -9.25 -6.75])
           (mg/sample [:float {:min -10 :max -5}] {:seed 0})
           (mg/sample [:float {:min -10 :max -5}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min -10] [:max -5]]}] (add-constraints {:seed 0}))
           (mg/sample [:float {:and [[:min -10] [:max -5] [:min -11] [:max -4]]}] (add-constraints {:seed 0}))))
    (is (every? (every-pred pos? float?) (mg/sample [:float {:min 0.0000001}] {})))
    (is (every? (every-pred pos? float?) (mg/sample [:float {:min 0.0000001}] (add-constraints {}))))
    (is (every? (every-pred neg? float?) (mg/sample [:float {:max -0.0000001}] {})))
    (is (every? (every-pred neg? float?) (mg/sample [:float {:max -0.0000001}] (add-constraints {}))))
    #?(:clj (testing "without constraints properties are checked for satisfiability"
              (is (thrown-with-msg?
                    AssertionError
                    (-> "(or (nil? lower-bound) (nil? upper-bound) (<= lower-bound upper-bound))"
                        java.util.regex.Pattern/quote
                        re-pattern)
                    (mg/generate [:float {:min 10 :max 9}] {})))))
    (testing "with constraints the solver signals unsatisfiability with zero solutions"
      (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-float-constraint"
          (mg/generate [:float {:min 10 :max 9}] (add-constraints {})))))))
