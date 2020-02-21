(ns diff-as-list.core
  (:require [clojure.test :as test]
            [clojure.set :as _set]
            [clojure.pprint :as pp]
            [clojure.data :refer [diff]]
            [clojure.test :as test]
            [clojure.string :as str]))

(def version "3.0.2")

(defn is-scalar? [val]
  (contains? #{"java.lang.String" "java.lang.Long" "clojure.lang.Keyword" "java.lang.Boolean"} (.getName (type val))))


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
         (do
           ;; below assoces result into a map
           (recur in-val
                  (rest remaining-traversal)
                  (first remaining-traversal)
                  (assoc result in-path {:type ::scalar :value value})))
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
         (throw (Exception. (str "don't know how to handle type " (.getName (type value))))))))))


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


(defn diffl [arg-1 arg-2]
  (let [flattened-1 (traverse-to-flat arg-1)
        flattened-2 (traverse-to-flat arg-2)
        keys-in-1 (set (keys flattened-1))
        keys-in-2 (set (keys flattened-2))
        keys-in-both (_set/intersection keys-in-1 keys-in-2)
        keys-in-both-compare (fn [_key]
                               (let [value-1 (get-in arg-1 _key ::not-found)
                                     val-1-map-or-prim (as-> flattened-1 $
                                                          (get $ _key)
                                                          (:type $))
                                     value-2 (get-in arg-2 _key ::not-found)
                                     val-2-map-or-prim (as-> flattened-2 $
                                                          (get $ _key)
                                                          (:type $))
                                     both-are-maps? (and (= ::map val-1-map-or-prim)
                                                         (= ::map val-2-map-or-prim))
                                     both-are-prims? (and (= ::scalar val-1-map-or-prim)
                                                          (= ::scalar val-2-map-or-prim))]
                                 (when-not (or both-are-maps?
                                               (and both-are-prims?
                                                    (= value-1 value-2)))
                                   (make-value-diff {:path _key :val-1 value-1 :val-2 value-2}))))
        value-diffs (as-> keys-in-both $
                      (map #(keys-in-both-compare %) $)
                      (remove nil? $)
                      (vec $))
        value-diff-paths (map :path value-diffs)
        is-path-child-of-other-path? (fn [path-1 path-2]
                                       (let [path-2-length (count path-2)
                                             path-1-length (count path-1)]
                                         (and (>= path-1-length path-2-length)
                                              (let [path-1-shortened (take path-2-length path-1)]
                                                (= path-2 path-1-shortened)))))
        is-missing-path-in-diffs? (fn [missing-path]
                                    (some #(is-path-child-of-other-path? missing-path %) value-diff-paths))
        remove-redundants (fn [keys-missing]
                            (let [sorted-by-length (sort-by count keys-missing)]
                              (reduce
                               (fn [accum key-path]
                                 (if (some #(is-path-child-of-other-path? key-path %) accum)
                                   accum
                                   (conj accum key-path)))
                               []
                               sorted-by-length)))
        keys-missing-in-2 (as-> (_set/difference keys-in-1 keys-in-2) $
                            (remove is-missing-path-in-diffs? $)
                            (remove-redundants $)
                            (map #(make-key-missing-diff {:path %
                                                          :value (get-in arg-1 %)
                                                          :missing-in :two}) $)
                            (vec $))
        keys-missing-in-1 (as-> (_set/difference keys-in-2 keys-in-1) $
                            (remove is-missing-path-in-diffs? $)
                            (remove-redundants $)
                            (map #(make-key-missing-diff {:path %
                                                          :value (get-in arg-2 %)
                                                          :missing-in :one}) $)
                            (vec $))]
    {:dal-version version
     :differences (into [] (concat value-diffs keys-missing-in-1 keys-missing-in-2))}))







(defn patch [orig-map _diff]
  (let [patch-reducing-fn
        (fn [accum single-diff]
          (let [#_ (clojure.pprint/pprint {:accum accum
                                          :single-diff single-diff})]
           (cond
             (instance? ValueDiff single-diff)
             (let [new-val (:val-2 single-diff)]
               (if (nil? accum)
                 new-val
                 (assoc-in accum (:path single-diff) new-val)))
             (and (instance? KeyMissingDiff single-diff)
                  (= (:missing-in single-diff) :one))
             (assoc-in accum (:path single-diff) (:value single-diff))
             (and (instance? KeyMissingDiff single-diff)
                  (= (:missing-in single-diff) :two))
             (let [removed-key-path (:path single-diff)]
               (if (<= (count removed-key-path) 1)
                 (dissoc accum (first removed-key-path))
                 (update-in accum (drop-last removed-key-path) dissoc (last removed-key-path))))
             :else (throw (RuntimeException. "should not be here")))))
        differences (map #(if (record? %)
                            %
                            (diff-map->record %))
                         (:differences _diff))]
    (reduce patch-reducing-fn orig-map differences)))
