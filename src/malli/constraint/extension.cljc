(ns malli.constraint.extension)

#_
[:atom [:map
        [:registry [:map-of Type IntoSchema]]
        [:extensions [:map-of Type [:map
                                    [:-walk
                                     {:optional true}
                                     [:=> [:maybe Schema] Schema Walker Path Constraint Options]]]]]]]
(defonce ^:private constraint-extensions (atom {}))

(defn get-constraint-extension [type]
  (get-in @constraint-extensions [:extensions type]))

(defn get-constraint [type]
  (get-in @constraint-extensions [:registry type]))

(defn register-constraints [reg]
  (swap! constraint-extensions update :registry #(merge % reg)))

(defn register-constraint-extensions! [extensions]
  (swap! constraint-extensions update :extensions #(merge-with into % extensions)))
