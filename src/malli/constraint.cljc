(ns malli.constraint)

#?(:cljs (goog-define mode "on")
   :clj  (def mode (or (System/getProperty "malli.constraint/mode") "on")))

(defprotocol IntoConstraint
  (-into-constraint [parent properties children options]))

(defn into-constraint?
  "Checks if x is a IntoConstraint instance"
  [x] (#?(:clj instance?, :cljs implements?) malli.constraint.IntoConstraint x))

(defprotocol Constraint
  (-constraint? [this])
  (-constraint-form [this]))

(defprotocol ConstrainedSchema
  (-constrained-schema? [this])
  (-get-constraint [this])
  (-set-constraint [this c]))

(extend-type #?(:clj Object, :cljs default)
  Constraint
  (-constraint? [_] false)
  (-intersect [_ _ _])

  ConstrainedSchema
  (-constrained-schema? [this] false)
  (-get-constraint [this])
  (-set-constraint [this c]))

(extend-type nil
  Constraint
  (-constraint? [_] false)
  (-constraint-form [_])

  ConstrainedSchema
  (-constrained-schema? [this] false)
  (-get-constraint [this])
  (-set-constraint [this c]))
