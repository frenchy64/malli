(ns malli.constraint.generator-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop' :refer [for-all]]
            [malli.core :as m]
            [malli.constraint :as mc]
            [malli.constraint.protocols :as mcp]
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
    ;;FIXME
    (is (thrown-with-msg?
          #?(:clj Exception, :cljs js/Error)
          #":malli\.generator/unsatisfiable-string-constraint"
          (mg/generate [:string {:min 10 :max 9}] (add-constraints {}))))))
