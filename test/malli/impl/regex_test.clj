(ns malli.impl.regex-test
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest check-ns-test
  (is (t/check-ns-clj 'malli.impl.regex))
  (is (t/check-ns-cljs 'malli.impl.regex)))
