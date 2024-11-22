(ns malli.simplify
  (:require [malli.core :as m]))

(defmulti intersect (fn [this _ _] (m/type this)))
(defmethod intersect :default [_ _ _])

(defmethod intersect :and
  [this that _]
  (when (= :and (m/type that))
    (m/-set-children this (into (m/children this) (m/children that)))))

(defn -intersect-min-max [this that]
  (when (= (m/type this) (m/type that))
    (let [p (m/properties this)
          p' (m/properties that)]
      (m/-set-properties this
                         (reduce-kv (fn [m k f]
                                      (if-some [v (let [l (k p) r (k p')] (if (and l r) (f l r) (or l r)))]
                                        (assoc m k v)
                                        m))
                                    nil {:min c/max :max c/min :gen/min c/max :gen/max c/min})))))

(defmethod intersect ::m/range-constraint [this that _] (m/-intersect-min-max this that))
(defmethod intersect ::m/count-constraint [this that _] (m/-intersect-min-max this that))

(defmethod intersect ::m/true-constraint [this that _] (when (= (m/type this) (m/type that)) this))
(defmethod intersect :any [this that _] (when (= (m/type this) (m/type that)) this))
(defmethod intersect ::m/false-constraint [this that _] (when (= (m/type this) (m/type that)) this))
(defmethod intersect :never [this that _] (when (= (m/type this) (m/type that)) this))

(defmulti simplify m/type)
(defmethod simplify :default [_ _ _])

(defn -intersect-common-constraints [cs]
  (->> cs
       (group-by type)
       (sort-by key)
       (into [] (mapcat (fn [[_ v]]
                          (case (count v)
                            1 (subvec v 0 1)
                            (let [[l r & nxt] v]
                              ;; if the first two intersect successfully, assume the rest do too
                              (if-some [in (intersect l r nil)]
                                [(if nxt
                                   (reduce #(intersect %1 %2 nil) in nxt)
                                   in)]
                                v))))))))

(defn -flatten-and [cs]
  (eduction (mapcat #(if (= :and (m/type %))
                       (m/children %)
                       [%]))
            cs))

;;TODO merge constraints into constrained schemas
(defmethod simplify :and
  [this]
  (let [ichildren (-> this m/children -flatten-and -intersect-common-constraints)]
    (when (not= ichildren (m/children this))
      (case (count ichildren)
        0 (m/schema :any (m/options this))
        1 (first ichildren)
        (m/-set-children ichildren)))))
