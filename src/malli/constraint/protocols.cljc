(ns malli.constraint.protocols)

(defprotocol Constraint
  (-constraint? [this]))

(extend-protocol Constraint
  Object
  (-constraint? [_] false))
