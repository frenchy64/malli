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
          #":malli\.generator/unsatisfiable-constraint"
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
          #":malli\.generator/unsatisfiable-constraint"
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
          #":malli\.generator/unsatisfiable-constraint"
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
          #":malli\.generator/unsatisfiable-constraint"
          (mg/generate [:float {:min 10 :max 9}] (add-constraints {})))))))

(deftest vector+sequential-constraint-generate-test
  (doseq [type [:vector :sequential]]
    (testing (str type " :and + :min + :max")
      (is (= [[] [-1] [-1 -1] [0 1] [1] [-3 -2] [-9 8 19] [1 0] [0 112 1] [-213 0 -36 -4 -40]]
             (mg/sample [type {} :int] {:seed 0})
             (mg/sample [type {} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and []} :int] (add-constraints {:seed 0}))))
      (is (= [[0 -1 0 -1 -1 -1 0 0 0 0] [0 0 0 0 0 0 0 -1 -1 0 0]
              [-1 0 -1 1 -1 0 1 1 0 -1 0 -1] [0 0 -4 1 -4 1 -1 -1 1 -2 0 1]
              [-2 -1 -2 1 0 0 0 0 -1 0 0] [-3 0 -1 7 5 -7 0 0 15 -14 1 -6]
              [-9 8 10 -1 1 -3 -1 -2 -2 1 -16 -1 -1] [1 -1 -1 -4 3 8 -1 -2 2 -8 3 -2]
              [0 112 -22 -6 3 -47 8 -3 1 35 -7 7 0] [-213 0 -36 -4 -2 -3 -11 -8 -4 -18 -4 -1 -184 -15 -2]]
             (mg/sample [type {:min 10} :int] {:seed 0})
             (mg/sample [type {:min 10} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:min 10]]} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:min 10] [:min 5]]} :int] (add-constraints {:seed 0}))))
      (is (= [[-1] [0 0 0 0 0] [-1 0 -1 1 -1 0 1 1 0 0] [0 0 -4 1 -4 1 -1] [-2 -1 -2 0]
              [-3 0 1] [-9 8 10 -2] [1 -1 23] [0 112 1] [-213 0 -36 -4 -40]]
             (mg/sample [type {:max 10} :int] {:seed 0})
             (mg/sample [type {:max 10} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:max 10]]} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:max 15] [:max 10]]} :int] (add-constraints {:seed 0}))))
      (is (= [[0 -1 0 -1] [0 0 0 0 0] [-1 0 -1 1 -1 -1] [0 0 -4 1 -4 -1] [-2 -1 -2 1 0]
              [-3 0 -1 7 0] [-9 8 10 -1 1] [1 -1 -1 0] [0 112 -22 -6 -1] [-213 0 -36 -4 -40]]
             (mg/sample [type {:min 4 :max 6} :int] {:seed 0})
             (mg/sample [type {:min 4 :max 6} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:min 4] [:max 6]]} :int] (add-constraints {:seed 0}))
             (mg/sample [type {:and [[:min 4] [:max 6] [:min 3] [:max 7]]} :int] (add-constraints {:seed 0}))))
      (is (every? seq (mg/sample [type {:min 1} :int] {})))
      (is (every? seq (mg/sample [type {:min 1} :int] (add-constraints {}))))
      (is (every? empty? (mg/sample [type {:max 0} :int] {})))
      (is (every? empty? (mg/sample [type {:max 0} :int] (add-constraints {}))))
      #?(:clj (testing "without constraints properties are checked for satisfiability"
                (is (thrown-with-msg?
                      AssertionError
                      (-> "(<= lower upper)"
                          java.util.regex.Pattern/quote
                          re-pattern)
                      (mg/generate [type {:min 10 :max 9} :int] {})))))
      (testing "with constraints the solver signals unsatisfiability with zero solutions"
        (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli\.generator/unsatisfiable-constraint"
            (mg/generate [type {:min 10 :max 9} :int] (add-constraints {}))))))))

#?(:clj (defn massage-seqable-sample [s]
          (mapv (fn [s]
                  (cond
                    (some-> s class .isArray) [::array (vec s)]
                    (and (instance? java.lang.Iterable s)
                         (not (instance? clojure.lang.IPersistentCollection s))) [::eduction (vec s)]
                    :else s))
                s)))

#?(:clj
   (deftest seqable-constraint-generate-test
     (testing ":and + :min + :max"
       (is (= [nil [::eduction [0]] #{} [::array [0]] [-2 2 0 1]
               [1 -2] [-9] [3 -49 -4] [-23 1 82] [::eduction [126 -24 -236 0 -18 0 0 2 -1]]]
              (massage-seqable-sample (mg/sample [:seqable {} :int] {:seed 0}))
              (massage-seqable-sample (mg/sample [:seqable {} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and []} :int] (add-constraints {:seed 0})))))
       (is (= [[-1 0 0 -1 -1 -1 0 0 0 -1] [::eduction [0 -1 0 -1 -1 -1 0 0 0 0 -1]] #{0 7 1 -2 4 -1 -6 -3 26 10}
               [::array [-2 -1 3 -2 1 0 -1 3 -3 -1 -1]] [-2 2 0 3 1 -2 -8 5 2 -3 3 -1 -2 -1]
               [1 -1 0 5 -5 0 -1 -1 1 -2 1 1] [::eduction [-1 6 -20 -1 4 -2 -4 1 -4 -1 2]]
               [3 -49 -2 7 -2 25 5 12 25 3 9 1 31] [-23 1 2 -51 -1 0 9 -2 -5 53 4 0 -4]
               [::eduction [126 -24 -236 0 -18 0 0 2 35 -105 -4 1 -1 -2 0 1 5 -37 -2]]]
              (massage-seqable-sample (mg/sample [:seqable {:min 10} :int] {:seed 0}))
              (massage-seqable-sample (mg/sample [:seqable {:min 10} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:min 10]]} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:min 10] [:min 5]]} :int] (add-constraints {:seed 0})))))
       (is (= [nil [::eduction [0 -1 0 -1 -1]] #{0} [::array [-2 -1 -1]] [-2 2 0 3 1 -2 -8 5 2 0]
               [1 -1 0 -1] [-1 5] [3 -49 -2 28] [-23 1 2 0] [::eduction [126 -24 -236 0 -18 0 0 2 35 -3]]]
              (massage-seqable-sample (mg/sample [:seqable {:max 10} :int] {:seed 0}))
              (massage-seqable-sample (mg/sample [:seqable {:max 10} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:max 10]]} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:max 15] [:max 10]]} :int] (add-constraints {:seed 0})))))
       (is (= [[-1 0 0 -1 -1] [::eduction [0 -1 0 -1 -1]] #{0 -2 -6 -3} [::array [-2 -1 3 1]] [-2 2 0 3 1 -6]
               [1 -1 0 5 -1] [::eduction [-1 6 -20 12]] [3 -49 -2 7 8] [-23 1 2 -51 3] [::eduction [126 -24 -236 0 -18 13]]]
              (massage-seqable-sample (mg/sample [:seqable {:min 4 :max 6} :int] {:seed 0}))
              (massage-seqable-sample (mg/sample [:seqable {:min 4 :max 6} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:min 4] [:max 6]]} :int] (add-constraints {:seed 0})))
              (massage-seqable-sample (mg/sample [:seqable {:and [[:min 4] [:max 6] [:min 3] [:max 7]]} :int]
                                                 (add-constraints {:seed 0})))))
       (is (every? seq (mg/sample [:seqable {:min 1} :int] {})))
       (is (every? seq (mg/sample [:seqable {:min 1} :int] (add-constraints {}))))
       (is (every? empty? (mg/sample [:seqable {:max 0} :int] {})))
       (is (every? empty? (mg/sample [:seqable {:max 0} :int] (add-constraints {}))))
       #?(:clj (testing "without constraints properties are checked for satisfiability"
                 (is (thrown-with-msg?
                       AssertionError
                       (-> "(<= lower upper)"
                           java.util.regex.Pattern/quote
                           re-pattern)
                       (mg/generate [:seqable {:min 10 :max 9} :int] {})))))
       (testing "with constraints the solver signals unsatisfiability with zero solutions"
         (is (thrown-with-msg?
               #?(:clj Exception, :cljs js/Error)
               #":malli\.generator/unsatisfiable-constraint"
               (mg/generate [:seqable {:min 10 :max 9} :int] (add-constraints {}))))))))

(deftest set-constraint-generate-test
  (testing ":and + :min + :max"
    (is (= [#{} #{0} #{0 -1} #{0 1} #{-1} #{-2 -17} #{-12 9 5} #{0 -1} #{4 -1 -3} #{0 -1 -8 237 6}]
           (mg/sample [:set {} :int] {:seed 0})
           (mg/sample [:set {} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and []} :int] (add-constraints {:seed 0}))))
    (is (= [#{0 -32 -1 -8 13 -15 3 -63 5 42} #{0 -505 1 -20 -2 -1 -3 -957 23 5 -307} #{0 -12 7 1 -2 -1 21 -6 172 -3 26 8}
            #{0 -4 1 -1 -6 201 17 3 2 -7 -115 -5} #{0 -4 -1 13 -23 -3 6 3 2 9 -9} #{0 1 -125 -2 4 -1 99 -3 -17 25 3 -13}
            #{0 -12 -28 1 -2 6 12 9 5 45 -9 -5 49} #{0 -4 -32 1 39 4 -1 -23 2 107 -168 16}
            #{0 -12 -4 4 -1 -6 33 13 -3 3 5 -29 -13} #{0 345 -1 15 -8 -3 237 -43 6 127 -235 -9 10 -5 1021}]
           (mg/sample [:set {:min 10} :int] {:seed 0})
           (mg/sample [:set {:min 10} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:min 10]]} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:min 10] [:min 5]]} :int] (add-constraints {:seed 0}))))
    (is (= [#{0} #{0 1 -20 -2 -1} #{0 -12 7 1 -2 -1 21 -6 -3 8} #{0 1 -1 17 3 2 -7} #{0 -1 13 9}
            #{-2 4 -17} #{-12 1 9 5} #{0 -1 -23} #{4 -1 -3} #{0 -1 -8 237 6}]
           (mg/sample [:set {:max 10} :int] {:seed 0})
           (mg/sample [:set {:max 10} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:max 10]]} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:max 15] [:max 10]]} :int] (add-constraints {:seed 0}))))
    (is (= [#{0 -1 -15 5} #{0 1 -20 -2 -1} #{0 7 -2 -1 -6 -3} #{0 1 -1 3 2 -7} #{0 -1 13 6 9}
            #{0 -2 4 -17 25} #{-12 1 9 5 -5} #{0 -1 -23 2} #{-4 4 -1 13 -3} #{0 -1 -8 237 6}]
           (mg/sample [:set {:min 4 :max 6} :int] {:seed 0})
           (mg/sample [:set {:min 4 :max 6} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:min 4] [:max 6]]} :int] (add-constraints {:seed 0}))
           (mg/sample [:set {:and [[:min 4] [:max 6] [:min 3] [:max 7]]} :int] (add-constraints {:seed 0}))))
    (is (every? seq (mg/sample [:set {:min 1} :int] {})))
    (is (every? seq (mg/sample [:set {:min 1} :int] (add-constraints {}))))
    (is (every? empty? (mg/sample [:set {:max 0} :int] {})))
    (is (every? empty? (mg/sample [:set {:max 0} :int] (add-constraints {}))))
    #?(:clj (testing "without constraints properties are checked for satisfiability"
              (is (thrown-with-msg?
                    AssertionError
                    (-> "(<= lower upper)"
                        java.util.regex.Pattern/quote
                        re-pattern)
                    (mg/generate [:set {:min 10 :max 9} :int] {})))))
    (testing "with constraints the solver signals unsatisfiability with zero solutions"
      (is (thrown-with-msg?
            #?(:clj Exception, :cljs js/Error)
            #":malli\.generator/unsatisfiable-constraint"
            (mg/generate [:set {:min 10 :max 9} :int] (add-constraints {})))))))
