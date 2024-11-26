;; demo
(ns malli.simplify
  (:require [malli.core :as m]
            [malli.solver :as solver]))

(defmulti -simplify-solution-type (fn [solution options] (:type solution)))
(defmethod -simplify-solution-type :number
  [{:keys [max-number min-number]} options]
  (if (or min-number max-number)
    (cond-> [:and number?]
      min-number (conj [:>= min-number])
      max-number (conj [:<= max-number]))
    number?))
(defmethod -simplify-solution-type nil [_ options] :any)

(defn simplify
  ([?schema] (simplify ?schema nil))
  ([?schema options]
   (let [schema (m/schema ?schema options)
         solutions (solver/solve schema options)
         ors (mapv #(-simplify-solution-type % options) solutions)]
     (m/schema
       (case (count ors)
         0 (m/-fail! ::unsatisfiable {:schema schema})
         1 (first ors)
         (into [:or] ors))
       options))))

(comment
  (simplify number?)
  (simplify [:and number? [:<= 10]])
  (simplify [:and number? [:<= 10] [:<= 20] [:<= 30]])
  ;;TODO
  (simplify [:and number? [:< 10]])
  )
