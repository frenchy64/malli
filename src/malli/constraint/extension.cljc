(ns malli.constraint.extension)

#_
[:atom [:map-of Type [:map
                      [:-walk
                       {:optional true}
                       [:=> [:maybe Schema] Schema Walker Path Constraint Options]]
                      ]]]
(defonce ^:private constraint-extensions (atom {}))

(defn get-constraint-extension [type]
  (@constraint-extensions type))

(defn register-constraint-extensions! [extensions]
  (swap! constraint-extensions #(merge-with into % extensions)))
