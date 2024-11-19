(ns malli.constraint
  (:require [malli.core :as m]
            [malli.constraint.and :refer [-and-constraint]]
            [malli.constraint.count :refer [-count-constraint]]
            [malli.constraint.ext.collection :as collection-ext]
            [malli.constraint.ext.number :as number-ext]
            [malli.constraint.ext.string :as string-ext]
            [malli.constraint.extension :as mce]
            [malli.constraint.protocols :as mcp]
            [malli.constraint.range :refer [-range-constraint]]
            [malli.constraint.true :refer [-true-constraint]]
            [malli.constraint.util :as mcu]
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

(defn with-base-constraints
  "Upgrade options with support for base constraints."
  [options]
  (-> options
      (update :malli.core/constraint-options #(merge-with into % (base-constraint-extensions)))))

(defn activate-base-constraints!
  "Upgrade default registry with support for the base constraints."
  []
  (mce/register-constraints (base-constraints))
  (mce/register-constraint-extensions! (base-constraint-extensions)))

(activate-base-constraints!)
