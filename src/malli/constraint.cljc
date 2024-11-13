(ns malli.constraint
  (:require [malli.constraint.and :refer [-and-constraint]]
            [malli.constraint.count :refer [-count-constraint]]
            [malli.constraint.ext.string :as string-ext]
            [malli.constraint.protocols :as mcp]
            [malli.constraint.true :refer [-true-constraint]]
            [malli.constraint.util :as mcu]
            [malli.registry :as mr]))

(defn base-constraint-extensions []
  (merge (string-ext/base-constraint-extensions)))

(defn base-constraints []
  {::count-constraint (-count-constraint)
   ::and (-and-constraint)
   ::true-constraint (-true-constraint)})

(let [base-ext! (delay (mcu/register-constraint-extensions! (base-constraint-extensions)))
      bc (delay (base-constraints))]
  (defn activate-base-constraints!
    ([] (mr/swap-default-registry! activate-base-constraints!))
    ([?registry]
     @base-ext! ;; hmm this will break the default registry if it doesn't also include (base-constraints)
     (mr/composite-registry @bc ?registry))))
