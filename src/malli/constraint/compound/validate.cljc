(ns malli.constraint.compound.validate
  (:require [malli.impl.util :as miu]))

(defn validators []
  {:and (fn [{:keys [constraint constraint-validator]} _]
          (let [ps (mapv constraint-validator (next constraint))]
            (when (empty? ps)
              (miu/-fail! ::empty-and))
            #(every? (fn [p] (p %)) ps)))})
