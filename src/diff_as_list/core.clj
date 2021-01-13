(ns diff-as-list.core
  (:require [clojure.set :as _set]
            [clojure.pprint :as pp]))

(def version "3.0.2")

(defn is-scalar? [val]
  (contains? #{"java.lang.String" "java.lang.Long"
               "clojure.lang.Keyword" "java.lang.Boolean"}
             (-> val type .getName)))


(defn traverse-to-flat
  ([in-val] (traverse-to-flat in-val [] [] {}))
  ([in-val remaining-traversal in-path result]
   ;; (pp/pprint {;; :in-val in-val
   ;;             :remaining-traversal remaining-traversal
   ;;             :in-path in-path
   ;;             :result result})
   (if (nil? in-path)
     result
     (let [value (get-in in-val in-path ::no-value)]
       (cond
         (or (nil? value) (is-scalar? value))
         (recur in-val
                (rest remaining-traversal)
                (first remaining-traversal)
                (assoc result in-path {:type ::scalar :value value}))
         (map? value)
         (let [_keys (keys value)
               first-key (first _keys)
               rest-keys (rest _keys)
               remain-trvrs-to-add (map #(conj in-path %) rest-keys)]
           ;; (pp/pprint {:remaining-traversal remaining-traversal
           ;;             :remain-trvrs-to-add remain-trvrs-to-add})
           (recur in-val
                  (concat remaining-traversal remain-trvrs-to-add)
                  (conj in-path first-key)
                  (assoc result in-path {:type ::map})))
         :else
         (throw (ex-info "don't know how to handle type"
                         {:type-name (-> value type .getName)})))))))


(defrecord KeyMissingDiff [type path value missing-in])
(defn make-key-missing-diff [key-missing-diff-in]
  (-> key-missing-diff-in
      (assoc :type ::KeyMissingDiff)
      map->KeyMissingDiff))

(defrecord ValueDiff [type path val-1 val-2])
(defn make-value-diff [value-diff-in]
  (-> value-diff-in
      (assoc :type ::ValueDiff)
      map->ValueDiff))

(defn diff-map->record [diff-item]
  (case (:type diff-item)
    ::ValueDiff
    (make-value-diff diff-item)
    ::KeyMissingDiff
    (make-key-missing-diff diff-item)
    (throw (RuntimeException. "should not be here"))))

(defn path->value-diff [{:keys [arg-1
                                arg-2
                                arg-1-flat
                                arg-2-flat]}
                        path]
  (let [value-1 (get-in arg-1 path ::not-found)
        val-1-map-or-prim (-> arg-1-flat
                              (get path)
                              :type)
        value-2 (get-in arg-2 path ::not-found)
        val-2-map-or-prim (-> arg-2-flat
                              (get path)
                              :type)
        both-are-maps? (and (= ::map val-1-map-or-prim)
                            (= ::map val-2-map-or-prim))
        both-are-prims? (and (= ::scalar val-1-map-or-prim)
                             (= ::scalar val-2-map-or-prim))]
    (when-not (or both-are-maps?
                  (and both-are-prims?
                       (= value-1 value-2)))
      (make-value-diff {:path path :val-1 value-1 :val-2 value-2}))))

(defn make-value-differences [{:keys [arg-1-paths arg-2-paths] :as diffl-ctx}]
  (let [keys-in-both (_set/intersection arg-1-paths arg-2-paths)]
   (->> keys-in-both
        (map #(path->value-diff diffl-ctx %))
        (remove nil?)
        vec)))

(defn is-path-child-of-other-path? [path-1 path-2]
  (let [path-2-length (count path-2)
        path-1-length (count path-1)]
    (and (>= path-1-length path-2-length)
         (let [path-1-shortened (take path-2-length path-1)]
           (= path-2 path-1-shortened)))))

(defn remove-redundants [keys-missing]
  (let [sorted-by-length (sort-by count keys-missing)]
    (reduce
     (fn [accum key-path]
       (if (some #(is-path-child-of-other-path? key-path %) accum)
         accum
         (conj accum key-path)))
     []
     sorted-by-length)))

(defn is-missing-path-in-diffs? [{:keys [value-diffs]} missing-path]
  (let [value-diff-paths (map :path value-diffs)]
    (some #(is-path-child-of-other-path? missing-path %)
          value-diff-paths)))

(defn make-key-missing-diffs [diffl-ctx
                              minuend-paths
                              subtrahend-paths
                              minuend-obj
                              subtrahend-name]
  (->> (_set/difference minuend-paths subtrahend-paths)
       (remove #(is-missing-path-in-diffs? diffl-ctx %))
       remove-redundants
       (map #(make-key-missing-diff {:path %
                                     :value (get-in minuend-obj %)
                                     :missing-in subtrahend-name}))
       vec))


(defn diffl [arg-1 arg-2]
  (let [arg-1-flat (traverse-to-flat arg-1)
        arg-2-flat (traverse-to-flat arg-2)
        arg-1-paths (-> arg-1-flat keys set)
        arg-2-paths (-> arg-2-flat keys set)
        diffl-ctx {:arg-1 arg-1
                   :arg-2 arg-2
                   :arg-1-flat arg-1-flat
                   :arg-2-flat arg-2-flat
                   :arg-1-paths arg-1-paths
                   :arg-2-paths arg-2-paths}
        value-diffs (make-value-differences diffl-ctx)
        diffl-ctx (assoc diffl-ctx :value-diffs value-diffs)
        keys-missing-in-2 (make-key-missing-diffs
                           diffl-ctx
                           arg-1-paths
                           arg-2-paths
                           arg-1
                           :two)
        keys-missing-in-1 (make-key-missing-diffs
                           diffl-ctx
                           arg-2-paths
                           arg-1-paths
                           arg-2
                           :one)]
    {:dal-version version
     :differences (into [] (concat value-diffs
                                   keys-missing-in-1
                                   keys-missing-in-2))}))







(defn patch [orig-map {:keys [differences] :as _diff}]
  (let [patch-reducing-fn
        (fn [accum {:keys [val-2 path missing-in value] :as single-diff}]
          (cond
            (instance? ValueDiff single-diff)
            (let [new-val val-2]
              (if-not accum
                new-val
                (assoc-in accum path new-val)))
            (and (instance? KeyMissingDiff single-diff)
                 (= missing-in :one))
            (assoc-in accum path value)
            (and (instance? KeyMissingDiff single-diff)
                 (= missing-in :two))
            (let [removed-key-path path]
              (if (<= (count removed-key-path) 1)
                (dissoc accum (first removed-key-path))
                (update-in accum
                           (drop-last removed-key-path)
                           dissoc
                           (last removed-key-path))))
            :else (throw (RuntimeException. "should not be here"))))
        differences (map #(if (record? %)
                            %
                            (diff-map->record %))
                         differences)]
    (reduce patch-reducing-fn orig-map differences)))
