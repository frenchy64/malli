(ns malli.constraint
  (:require [malli.constraint.and :refer [-and-constraint]]
            [malli.constraint.count :refer [-count-constraint]]
            [malli.constraint.ext.collection :as collection-ext]
            [malli.constraint.ext.number :as number-ext]
            [malli.constraint.ext.string :as string-ext]
            [malli.constraint.extension :as mce]
            [malli.constraint.protocols :as mcp]
            [malli.constraint.range :refer [-range-constraint]]
            [malli.constraint.true :refer [-true-constraint]]
            [malli.constraint.util :as mcu]
            [malli.core :as-alias m]
            [malli.registry :as mr]))

(defn base-constraint-extensions []
  (merge (number-ext/base-constraint-extensions)
         (string-ext/base-constraint-extensions)
         (collection-ext/base-constraint-extensions)))

(defn base-constraints []
  {::range-constraint (-range-constraint)
   ::count-constraint (-count-constraint)
   ::and (-and-constraint)
   ::true-constraint (-true-constraint)})

(defn activate-base-constraints!
  "Upgrade default registry with support for the base constraints."
  []
  (let [bc (base-constraints)
        _ (mce/register-constraint-extensions! (base-constraint-extensions))]
    (mr/swap-default-registry! #(mr/composite-registry bc %))))

(defn with-base-constraints
  "Upgrade options with support for base constraints."
  [options]
  (-> options
      (update ::m/constraint-options #(merge-with into % (base-constraint-extensions)))
      (update :registry #(mr/composite-registry (base-constraints) %))))
