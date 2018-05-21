(ns diff-as-list.attempt-2
  (:require [clojure.test :as test]
            [clojure.set :as _set]
            [clojure.pprint :as pp]))

(defn- is-primitive? [val]
  (contains? #{"java.lang.String" "java.lang.Long" "clojure.lang.Keyword"} (.getName (type val))))

;; (deftest primitive
;;   (is (= true (is-primitive? "test")))
;;   (is (is-primitive? 1))
;;   (is (not (is-primitive? {})))
;;   (is (not (is-primitive? []))))


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
         (or (nil? value) (is-primitive? value))
         (do
           ;; below conjes result into a vector
           ;; (recur in-val (rest remaining-traversal) (first remaining-traversal) (conj result {:path in-path :value value}))
           ;; below assoces result into a map
           (recur in-val
                  (rest remaining-traversal)
                  (first remaining-traversal)
                  (assoc result in-path {:type ::primitive :value value}))
           )
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
         (throw (Exception. "don't know how to handle type " (.getName (type value)))))))
   ))


(println "\n\n---- new compile ----")


(def test-map {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                       :level-3-2 {:level-4-1 "level-4-1-val"}}
                           :level-2-2 "level-2-2-val"
                           :level-2-3 "level-2-3-val"}
               :level-1-2 "level-1-2-val"})

(println "\ntest-map")
(pp/pprint test-map)
(def test-map-flattened (traverse-to-flat test-map))
(println "\ntest-map-flattened")
(pp/pprint test-map-flattened)

(def test-map-2 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                         ;; :level-3-2 nil
                                         }
                             :level-2-2 "level-2-2-val"
                             :level-2-3 "level-2-3-val"}
                 :level-1-2 "level-1-2-val"})

(println "\ntest-map-2")
(pp/pprint test-map-2)
(def test-map-2-flattened (traverse-to-flat test-map-2))
(println "\ntest-map-2-flattened")
(pp/pprint test-map-2-flattened)


;; (def diffed (clojure.data/diff test-map test-map-2))

;; (println "\ndiffed")
;; (pp/pprint diffed)

(defn diffl [arg-1 arg-2]
  (let [flattened-1 (traverse-to-flat arg-1)
        flattened-2 (traverse-to-flat arg-2)
        keys-in-1 (set (keys flattened-1))
        keys-in-2 (set (keys flattened-2))
        keys-missing-in-2 (_set/difference keys-in-1 keys-in-2)
        keys-missing-in-1 (_set/difference keys-in-2 keys-in-1)
        keys-in-both (_set/intersection keys-in-1 keys-in-2)
        keys-in-both-compare (fn [_key]
                               (let [value-1 (get-in arg-1 _key ::not-found)
                                     value-2 (get-in arg-2 _key ::not-found)]
                                 (when (not (= value-1 value-2))
                                   {:path _key :val-1 value-1 :val-2 value-2})))]
    {:keys-missing-in-1 keys-missing-in-1
     :keys-missing-in-2 keys-missing-in-2
     :value-differences (remove nil? (map #(keys-in-both-compare %) keys-in-both))}
    ))

(def diffl-val (diffl test-map test-map-2))
(println "\ndiffl-val")
(pp/pprint diffl-val)
