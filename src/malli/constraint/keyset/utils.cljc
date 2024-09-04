(ns malli.constraint.keyset.utils
  (:require [malli.impl.util :as miu]))

(defn eval-dispatch [dispatch options]
  (when-not (keyword? dispatch)
    (miu/-fail! ::non-keyword-dispatch-constraint-nyi {:dispatch dispatch}))
  dispatch)
