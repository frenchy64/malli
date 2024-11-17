(ns malli.dev.constraint
  (:require [malli.core :as m]
            ;; m/=> doesn't seem to support aliases
            malli.constraint
            malli.constraint.extension
            malli.constraint.ext.string
            malli.constraint.ext.number
            malli.constraint.range
            malli.constraint.count))

(def Schema :any)
(def Properties :any)
(def Form :any)
(def ?Constraint :any)
(def ContextualConstraintForm :any)
(def Type :any)
(def Walker :any)
(def Path :any)
(def Constraint :any)
(def Options [:maybe map?])
(def ConstraintExtension
  [:map
   ;; a function taking a Schema's properties and returning a Constraint.
   ;; e.g., [:string {:max 1}] => [::mc/count-constraint 0 1]
   [:constraint-from-properties
    [:-> Properties Options Constraint]]
   ;; a function taking surface-syntax for a constraint and returning a Constraint.
   ;; e.g., :string's [:max 5] => [::mc/count-constraint 0 5]
   ;; e.g., :int's [:max 5] => [::mc/range-constraint {} nil 5]
   [:parse-constraint
    {:optional true}
    [:map-of :any [:->
                   ;; for [:max {:foo 1} 5], will be passed:
                   ;; {:properties {:foo 1} :children [5]}
                   ;; the function will be registered under :max.
                   [:map
                    [:properties Properties]
                    [:children [:sequential :any]]]
                   Options
                   ?Constraint]]]
   ;; a function to return the form of a constraint under the current schema.
   ;; e.g., :string's [:max 5] <= [::mc/count-constraint 0 5]
   ;; e.g., :int's [:max 5] <= [::mc/range-constraint {} nil 5]
   [:constraint-form
    {:optional true}
    [:map-of Type [:-> Constraint Options Form]]]
   ;; a function to parse a property into a contextual constraint form.
   ;; e.g., [:string {:max 1}] => [:max 1]
   ;; e.g., [:string {:and [[:max 1]]}] => [:and [:max 1]]
   [:parse-properties
    [:map-of :any [:-> :any Options ContextualConstraintForm]]]
   ;; a function to convert a Constraint back to the properties of its schema.
   ;; e.g., [:string {:max 4}] <= [::mc/count-constraint 0 4]
   ;; e.g., [:int {:max 4}] <= [::mc/range-constraint 0 4]
   [:unparse-properties
    [:-> Constraint Properties Options Properties]]
   ;; a custom walking function to walk both schema and its constraints.
   [:-walk
    {:optional true}
    [:-> Schema Walker Path Constraint Options [:maybe Schema]]]])

(def ConstraintExtensions [:map-of Type ConstraintExtension])

(m/=> malli.constraint.extension/get-constraint-extension [:-> :any [:maybe ConstraintExtension]])
(m/=> malli.constraint.extension/register-constraint-extensions! [:-> ConstraintExtensions ConstraintExtensions])
(m/=> malli.constraint/base-constraint-extensions [:-> ConstraintExtensions])
(m/=> malli.constraint/base-constraints [:-> ConstraintExtensions])
(m/=> malli.constraint.ext.string/base-constraint-extensions [:-> ConstraintExtensions])
(m/=> malli.constraint.ext.number/base-constraint-extensions [:-> ConstraintExtensions])
(m/=> malli.constraint.range/default-constraint-extensions [:-> ConstraintExtension])
(m/=> malli.constraint.count/default-constraint-extensions [:-> ConstraintExtension])
