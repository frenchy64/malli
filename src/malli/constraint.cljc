(ns malli.constraint)

#?(:cljs (goog-define mode "on")
   :clj  (def mode (or (System/getProperty "malli.constraint/mode") "on")))

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
