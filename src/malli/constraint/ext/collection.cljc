(ns malli.constraint.ext.collection
  (:require [malli.core :as m]
            [malli.constraint :as-alias mc]
            [malli.constraint.util :as mcu]
            [malli.constraint.count :as count]))

(defn -base-collection-extension []
  (-> (mcu/default-constraint-extensions)
      ;TODO
      ;(assoc :-walk mcu/-walk-leaf+constraints)
      (as-> $ (merge-with into $ (count/default-constraint-extensions)))))

(defn base-constraint-extensions []
  (let [ext (-base-collection-extension)]
    {:vector ext :sequential ext :seqable ext :set ext
     ;;TODO :every (bounded)
     }))
