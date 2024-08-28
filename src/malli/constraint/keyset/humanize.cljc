(ns malli.constraint.keyset.humanize
  (:require [malli.core :as-alias m]
            [malli.constraint.keyset.utils :as mcku]
            [malli.impl.util :as miu]))

(defn humanizers []
  {;;TODO [:not :disjoint]
   :disjoint (fn [{:keys [constraint constraint-validator humanize-constraint-violation
                          value]}
                  _]
               (let [ksets (subvec constraint 1)
                     has? #(contains? value %)
                     [has-constraint has-k] (some (fn [i]
                                                    (when-some [[has-k] (not-empty
                                                                          (filter has? (nth ksets i)))]
                                                      [i has-k]))
                                                  (range (count ksets)))
                     violating-ks (filterv has?
                                           (apply concat (subvec ksets (inc has-constraint))))]
                 (str "should not combine key " (pr-str has-k)
                      " with key" (if (next violating-ks) "s" "") ": "
                      (apply str (interpose " " (map pr-str violating-ks))))))
   :contains (fn [{:keys [constraint validator value]} _]
               (when-not (validator value)
                 (str "should provide key: " (pr-str (nth constraint 1)))))
   [:not :contains] (fn [{:keys [constraint validator value]} _]
                      (when-not (validator value)
                        (str "should not provide key: " (pr-str (nth constraint 1)))))
   :dispatch (fn [{:keys [constraint value constraint-validator humanize-constraint-violation]} options]
               (when-not (next constraint)
                 (miu/-fail! ::dispatch-constraint-must-supply-dispatch-fn {:constraint constraint}))
               (let [[_ dispatch & clauses] constraint
                     dispatch (mcku/eval-dispatch dispatch options)
                     {::m/keys [default] :as dispatch-map} (into {} clauses)
                     c (dispatch-map (dispatch value) default)]
                 (when-not ((constraint-validator c) value)
                   (humanize-constraint-violation c))))})