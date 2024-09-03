(ns malli.constraint.compound.humanize
  (:require [malli.error.utils :refer [-flatten-errors]]))

(defn humanizers []
  {:and (fn [{:keys [constraint constraint-validator
                     humanize-constraint-violation value]} _]
          (-flatten-errors
            (into [:and] (keep (fn [constraint]
                                 (let [validator (constraint-validator constraint)]
                                   (when-not (validator value)
                                     (humanize-constraint-violation constraint)))))
                  (next constraint))))})
