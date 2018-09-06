(ns diff-as-list.core
  (:require [clojure.test :as test]
            [clojure.set :as _set]
            [clojure.pprint :as pp]
            [clojure.data :refer [diff]]
            [clojure.test :as test]
            [clojure.string :as str]))

  (def version "2.2.5")

(defn- is-primitive? [val]
  (contains? #{"java.lang.String" "java.lang.Long" "clojure.lang.Keyword"} (.getName (type val))))

(test/deftest primitive
  (test/is (= true (is-primitive? "test")))
  (test/is (is-primitive? 1))
  (test/is (not (is-primitive? {})))
  (test/is (not (is-primitive? []))))


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
           ;; below assoces result into a map
           (recur in-val
                  (rest remaining-traversal)
                  (first remaining-traversal)
                  (assoc result in-path {:type ::primitive :value value})))
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


;; (println "\n\n---- new compile ----")


;; (def test-map {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
;;                                        :level-3-2 {:level-4-1 "level-4-1-val"}}
;;                            :level-2-2 "level-2-2-val"
;;                            :level-2-3 "level-2-3-val"}
;;                :level-1-2 "level-1-2-val"})

;; (println "\ntest-map")
;; (pp/pprint test-map)
;; (def test-map-flattened (traverse-to-flat test-map))
;; (println "\ntest-map-flattened")
;; (pp/pprint test-map-flattened)

;; (def test-map-2 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
;;                                          :level-3-2 nil
;;                                          }
;;                              :level-2-2 "level-2-2-val"
;;                              :level-2-3 "level-2-3-val"}
;;                  :level-1-2 "level-1-2-val"})

;; (println "\ntest-map-2")
;; (pp/pprint test-map-2)
;; (def test-map-2-flattened (traverse-to-flat test-map-2))
;; (println "\ntest-map-2-flattened")
;; (pp/pprint test-map-2-flattened)


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
                                     both-are-prims? (and (= ::primitive val-1-map-or-prim)
                                                          (= ::primitive val-2-map-or-prim))]
                                 (when-not (or both-are-maps?
                                               (and both-are-prims?
                                                    (= value-1 value-2)))
                                   {:path _key :val-1 value-1 :val-2 value-2})))
        value-diffs (as-> keys-in-both $
                      (map #(keys-in-both-compare %) $)
                      (remove nil? $)
                      (vec $))
        value-diff-paths (map :path value-diffs)
        is-path-child-of-other-path? (fn [path-1 path-2]
                                       (let [path-2-length (count path-2)
                                             path-1-length (count path-1)]
                                         (if (< path-1-length path-2-length)
                                           false
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
                            (map #(hash-map :path % :value (get-in arg-1 %)) $)
                            (vec $))
        keys-missing-in-1 (as-> (_set/difference keys-in-2 keys-in-1) $
                            (remove is-missing-path-in-diffs? $)
                            (remove-redundants $)
                            (map #(hash-map :path % :value (get-in arg-2 %)) $)
                            (vec $))]
    {:keys-missing-in-1 keys-missing-in-1
     :keys-missing-in-2 keys-missing-in-2
     :value-differences value-diffs
     :dal-version version}))

(defn- map-is-same? [map1 map2]
  (let [the-diff (diff map1 map2)]
    (and (nil? (first the-diff)) (nil? (nth the-diff 1)))))

;; (def diffl-val (diffl test-map test-map-2))
;; (println "\ndiffl-val")
;; (pp/pprint diffl-val)

(test/deftest initial
  (let [expected {:keys-missing-in-1 [],
                  :keys-missing-in-2 [],
                  :value-differences [{:path [:what], :val-1 "nothing", :val-2 "who"}]}
        actual (-> (diffl {:what "nothing"} {:what "who"})
                   (dissoc :dal-version))]
    (test/is (map-is-same? expected actual))))


(test/deftest nil-arg
  (let [expected {:keys-missing-in-1 [],
                  :keys-missing-in-2 [],
                  :value-differences [{:path [:what], :val-1 nil, :val-2 {:who "why"}}]}
        actual (-> (diffl {:what nil} {:what {:who "why"}})
                   (dissoc :dal-version))]
    (test/is (map-is-same? expected actual))))

(test/deftest missing-key
  (let [map-1 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                       :level-3-2 {:level-4-1 "level-4-1-val"}}
                           :level-2-2 "level-2-2-val"
                           :level-2-3 "level-2-3-val"}
               :level-1-2 "level-1-2-val"}
        map-2 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"}
                             :level-2-2 "level-2-2-val"
                             :level-2-3 "level-2-3-val"}
               :level-1-2 "level-1-2-val"}
        expected {:keys-missing-in-1 [],
                  :keys-missing-in-2 [
                                      {:path [:level-1-1 :level-2-1 :level-3-2]
                                       :value {:level-4-1 "level-4-1-val"}}],
                  :value-differences []}
        actual (-> (diffl map-1 map-2)
                   (dissoc :dal-version))]
    ;; (pp/pprint actual)
    (test/is (map-is-same? expected actual))))

(test/deftest remove-redundant
  (let [map-1 {:question-section
               {:name "question-section",
                :fields {:text {:name "text", :type :character, :max-length 200}}}}
        map-2 {:question-section
               {:name "question-section",
                :fields
                {:text {:name "text", :type :character, :max-length 200},
                 :bogus {:name "bogus", :type :integer},
                 :what {:name "what", :type :djunk}}}}
        expected {:keys-missing-in-1
                  [{:path [:question-section :fields :bogus],
                    :value {:name "bogus", :type :integer}}
                   {:path [:question-section :fields :what],
                    :value {:name "what", :type :djunk}}],
                  :keys-missing-in-2 [],
                  :value-differences []}
        actual (-> (diffl map-1 map-2)
                   (dissoc :dal-version))]
    ;; (println "\nexpected:")
    ;; (pp/pprint expected)
    ;; (println "\n:actual:")
    ;; (pp/pprint actual)
    (test/is (map-is-same? expected actual))))







(defn patch [orig-map _diff]
  (let [apply-value-diff (fn [accum value-diff]
                           (let [new-val (:val-2 value-diff)]
                             (if (nil? accum)
                               new-val
                               (assoc-in accum (:path value-diff) new-val))))
        remove-key (fn [accum removed-key]
                     (let [removed-key-path (:path removed-key)]
                       (if (<= (count removed-key-path) 1)
                         (dissoc accum (first removed-key-path))
                         (update-in accum (drop-last removed-key-path) dissoc (last removed-key-path)))))
        add-key (fn [accum added-key]
                  (assoc-in accum (:path added-key) (:value added-key)))]
    (as-> orig-map $
      ;; (if (nil? $) {} $)
      (reduce apply-value-diff $ (:value-differences _diff))
      (reduce remove-key $ (:keys-missing-in-2 _diff))
      (reduce add-key $ (:keys-missing-in-1 _diff))
      )))





(test/deftest patch-initial
  (let [orig-map {:what "nothing"}
        _diff {:keys-missing-in-1 [],
               :keys-missing-in-2 [],
               :value-differences [{:path [:what], :val-1 "nothing", :val-2 "who"}]}
        expected {:what "who"}
        actual (patch orig-map _diff)]
    ;; (pp/pprint actual)
    (test/is (= expected actual))))

(test/deftest patch-nil-arg
  (let [orig-map {:what nil}
        _diff {:keys-missing-in-1 [],
               :keys-missing-in-2 [],
               :value-differences [{:path [:what], :val-1 nil, :val-2 {:who "why"}}]}
        expected {:what {:who "why"}}
        actual (patch orig-map _diff)]
    (test/is (= expected actual))))


(test/deftest patch-key-removed
  (let [orig-map {:what "one"
                  :who "two"}
        _diff {:keys-missing-in-1 []
               :keys-missing-in-2 [{:path [:who] :value "two"}]
               :value-differences []}
        expected {:what "one"}
        actual (patch orig-map _diff)]
    (test/is (= expected actual))))

(test/deftest patch-key-removed-nested
  (let [orig-map {:what {:why "who"}}
        _diff {:keys-missing-in-1 []
               :keys-missing-in-2 [{:path [:what :why] :value "who"}]
               :value-differences []}
        expected {:what {}}
        actual (patch orig-map _diff)
        ]
    (test/is (= expected actual))))


;; (def/deftest patch-added-key) !!!!!!
(test/deftest patch-key-added
  (let [orig-map {:what {}}
        _diff {:keys-missing-in-1 [{:path [:what :why] :value "who"}]
               :keys-missing-in-2 []
               :value-differences []}
        expected {:what {:why "who"}}
        actual (patch orig-map _diff)]
    (test/is (= expected actual))))


(test/deftest patch-nil-to-value
  (let [orig-map nil
        _diff {:keys-missing-in-1 (), :keys-missing-in-2 (),
               :value-differences [{:path [], :val-1 nil, :val-2 {:what "who"}}]}
        expected {:what "who"}
        actual (patch orig-map _diff)
        ]
    (test/is (= expected actual))))
