(ns malli.constraint.ext.number
  (:require [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.constraint.range :as range]
            [malli.constraint.util :as mcu]))

(defn -base-number-extension []
  (-> (mcu/default-constraint-extensions)
      (assoc :-walk mcu/-walk-leaf+constraints)
      (as-> $ (merge-with into $ (range/default-constraint-extensions)))))

(defn base-constraint-extensions []
  (let [ext (-base-number-extension)]
    {:int ext :double ext :float ext}))
