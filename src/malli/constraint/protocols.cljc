(ns malli.constraint.protocols)

(defprotocol IntoConstraint
  (-type [this] "returns type of the constraint")
  (-into-constraint [this properties children options] "creates a new schema instance"))

(defprotocol Constraint
  (-validator [this options] "returns a predicate function that checks if a value satisfies constraint")
  (-humanizer [this path options] "returns a function of `x in acc -> maybe errors` to humanize the errors for invalid values")
  (-walk [this walker path options] "walks the constraint and its children, ::m/walk-entry-vals, ::m/walk-refs, ::m/walk-schema-refs options effect how walking is done.")
  (-properties [this options] "returns original constraint properties")
  (-options [this options] "returns original options")
  (-children [this options] "returns constraint children")
  (-parent [this options] "returns the IntoConstraint instance")
  (-form [this options] "returns original form of the constraint"))

(defprotocol ConstraintWalker
  (-accept [this constraint path options])
  (-inner [this constraint path options])
  (-outer [this constraint path children options]))

(comment
  [:string {:min 1}]
  {:type :string
   :constraints {:type :min-count
                 :children 1}}
  [:string {:min 1}]
  )
