(ns malli.constraint.protocols)

(defprotocol Constraint
  (-constraint? [this]))

(defprotocol ConstrainedSchema
  (-constrained-schema? [this])
  (-get-constraint [this])
  (-set-constraint [this c]))

(extend-type Object
  Constraint
  (-constraint? [_] false)

  ConstrainedSchema
  (-constrained-schema? [this] false)
  (-get-constraint [this])
  (-set-constraint [this c]))
