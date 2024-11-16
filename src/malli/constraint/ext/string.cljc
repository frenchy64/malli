(ns malli.constraint.ext.string
  (:require [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.constraint.util :as mcu]
            [malli.constraint.count :as count]))

(defn base-constraint-extensions []
  {:string (-> (mcu/default-constraint-extensions)
               (assoc :-walk mcu/-walk-leaf+constraints)
               (as-> $ (merge-with into $ (count/default-constraint-extensions))))})
