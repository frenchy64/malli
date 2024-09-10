(ns malli.constraint.protocols)

(defprotocol Constraint
  (-constraint? [this])
  (-intersect [this that options]))

(defprotocol ConstrainedSchema
  (-constrained-schema? [this])
  (-get-constraint [this])
  (-set-constraint [this c]))

(extend-type Object
  Constraint
  (-constraint? [_] false)
  (-intersect [_ _ _])

  ConstrainedSchema
  (-constrained-schema? [this] false)
  (-get-constraint [this])
  (-set-constraint [this c]))
