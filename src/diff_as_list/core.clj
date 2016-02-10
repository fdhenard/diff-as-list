(ns diff-as-list.core
  (:require [clojure.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.test :refer :all]
            [clojure.set :as _set]
            [clojure.data :refer [diff]]
            [clojure.zip :as zip]))

(defn- is-primitive? [val]
  (contains? #{"java.lang.String" "java.lang.Long" "clojure.lang.Keyword"} (.getName (type val))))

(deftest primitive
  (is (= true (is-primitive? "test")))
  (is (is-primitive? 1))
  (is (not (is-primitive? {})))
  (is (not (is-primitive? []))))

(defn- is-func? [obj]
  (= (.getName (type obj)) "clojure.lang.Fn"))

(defn- find-match [obj coll ident-func]
  (loop
      [unmatched coll]
    (if (empty? unmatched)
      [obj nil]
      (if (= (ident-func obj) (ident-func (first unmatched)))
        [obj (first unmatched)]
        (recur (rest unmatched))
        ))))

(defn diffl
  ([arg-1 arg-2] (diffl arg-1 arg-2 nil [] []))
  ([arg-1 arg-2 depth-ident-funcs] (diffl arg-1 arg-2 depth-ident-funcs [] []))
  ([arg-1 arg-2 depth-ident-funcs keypath keypath-w-id]
   ;; (pp/pprint {:arg-1 arg-1
   ;;             :arg-2 arg-2
   ;;             :keypath keypath})
   (if (or (nil? arg-1) (nil? arg-2))
     (if (and (nil? arg-1) (nil? arg-2))
       []
       [{:keypath keypath
         :keypath-w-id keypath-w-id
         :value-1 arg-1
         :value-2 arg-2}])
     (let [arg-1-type (.getName (type arg-1))
           arg-2-type (.getName (type arg-2))]
       (when (not (= arg-1-type arg-2-type))
         [{:keypath keypath
           :keypath-w-id keypath-w-id
           :typediff {:arg-1-type arg-1-type :arg-2-type arg-2-type}}])
       (cond
         (map? arg-1)
         (let [keys-in-1 (set (keys arg-1))
               keys-in-2 (set (keys arg-2))
               keys-missing-in-2 (_set/difference keys-in-1 keys-in-2)
               keys-missing-in-1 (_set/difference keys-in-2 keys-in-1)
               keys-in-both (_set/intersection keys-in-1 keys-in-2)
               initial-difference (if (or (> (count keys-missing-in-1) 0)
                                          (> (count keys-missing-in-2) 0))
                                    [{:keypath keypath
                                      :keypath-w-id keypath-w-id
                                      :keys-missing-in-1 keys-missing-in-1
                                      :keys-missing-in-2 keys-missing-in-2}
                                     ]
                                    [])]
           (flatten (into initial-difference
                          (map #(diffl (% arg-1) (% arg-2) depth-ident-funcs (conj keypath %) (conj keypath-w-id %)) keys-in-both))))
         (is-primitive? arg-1)
         (if (not (= arg-1 arg-2))
           (do
             ;; (println (format "diff found at %s: 1 = %s; 2 = %s" keypath arg-1 arg-2))
             [{:keypath keypath
               :keypath-w-id keypath-w-id
               :value-1 arg-1
               :value-2 arg-2}])
           [])
         (sequential? arg-1)
         (let [depth (count keypath)]
           (if (nil? depth-ident-funcs)
             (throw (Exception. (format "Encountered a vector at depth %s, but an empty depth-ident func. keypath = %s" depth keypath)))
             (let [ident-func (get depth-ident-funcs keypath)]
               (if (nil? ident-func)
                 (throw (Exception. (format "Could not find a depth-ident func for depth %s; keypath = %s" depth keypath)))
                 (let [matches-for-1 (map #(find-match % arg-2 ident-func) arg-1)
                       matches-for-2 (map #(find-match % arg-1 ident-func) arg-2)
                       unmatched-for-2 (map (fn [[x y]] [y x]) (filter #(nil? (second %)) matches-for-2))
                       matches (concat matches-for-1 unmatched-for-2)]
                   (flatten (map (fn [[a b]] (diffl a b depth-ident-funcs keypath (conj keypath-w-id (ident-func a)))) matches)))))))
         :else
         (throw (Exception. (str "don't know how to handle type " (.getName (type arg-1))))))))))

(defn- map-is-same? [map1 map2]
  (let [the-diff (diff map1 map2)]
    (and (nil? (first the-diff)) (nil? (nth the-diff 1)))))

(deftest attempt
  (let [expected-diff {:keypath [:what]
                       :keypath-w-id [:what]
                       :value-1 "nothing"
                       :value-2 "who"}
        actual (diffl {:what "nothing"} {:what "who"})]
    (is (= (count actual) 1))
    (is (map-is-same? expected-diff (first actual)))))

(deftest nil-arg
  (let [expected-diff {:keypath [:what]
                       :keypath-w-id [:what]
                       :value-1 nil
                       :value-2 {:who "why"}}
        actual (diffl {:what nil} {:what {:who "why"}})]
    (is (= (count actual) 1))
    (is (map-is-same? expected-diff (first actual)))
    ;;(pp/pprint actual)
    ))

(deftest in-neither
  (let [val-1 [{:one 1
                :two 2}]
        val-2 [{:one 2
                :two 3}]
        actual (diffl val-1 val-2 {[] #(:one %)})]
    ;; (pp/pprint actual)
    (is (= 2 (count actual)))
    (let [in-1 (filter #(not (nil? (:value-1 %))) actual)
          in-2 (filter #(not (nil? (:value-2 %))) actual)]
      (is (= 1 (count in-1)))
      (is (= 1 (count in-2)))
      (is (map-is-same? (-> in-1 first :value-1) (first val-1)))
      (is (map-is-same? (-> in-2 first :value-2) (first val-2))))))

(deftest deeper
  (is (map-is-same?
       {:keypath [:what :who]
        :keypath-w-id [:what :who]
        :value-1 "one"
        :value-2 "two"}
       (first (diffl {:what {:who "one"}} {:what {:who "two"}})))))

(deftest my-diff-with-vectors
  (let [val-1 [{:two 2
                :three "three"}]
        val-2 [{:two 2
                :three "four"}]
        expected-diff {:keypath [:three]
                       :keypath-w-id [2 :three]
                       :value-1 "three"
                       :value-2 "four"}]
    (is (map-is-same? expected-diff
                      (first (diffl val-1 val-2 {[] #(:two %)}))))))

(defn- get-map-from-seq [coll ident-func]
  (first (filter ident-func coll)))

(defn- my-get-in
  ([obj keypath] (my-get-in obj keypath 0))
  ([obj keypath depth]
   (pp/pprint {:obj obj
               :obj-type (.getName (type obj))
               :keypath keypath
               :depth depth})
   (if (= 1 (count keypath))
     (cond
       (map? obj)
       ((first keypath) obj)
       (sequential? obj)
       (let [key-type-name (.getName (type (first keypath)))]
         (if (is-func? (first keypath))
           (first (filter (first keypath) obj))
           (throw (Exception. (str "Need a function to know how to pick from a list.  type is " key-type-name)))))
       :else
       (throw (Exception. (str "Only know how to handle map or vector, obj type is" (.getName (type obj))))))
     (cond
       (map? obj)
       (recur ((first keypath) obj) (take-last (- (count keypath) 1) keypath) (+ depth 1))
       (sequential? obj)
       (recur (first (filter (first keypath) obj)) (take-last (- (count keypath) 1) keypath) (+ depth 1))))))

(deftest test-my-get-in
  (let [expected "two"]
    (is (= "two"
           (my-get-in {:one {:two "two"}} [:one :two])))
    (is (= "tttt"
           (my-get-in {:one {:two {:three "tttt"}}} [:one :two :three])))))


(deftest test-my-get-in-with-list
  (is (= 4
         (my-get-in {:one [{:two 2
                            :three {:four 4}}
                           {:five "five"
                            :six 6}]}
                    [:one
                     #(and (contains? % :two) (= (:two %) 2))
                     :three
                     :four]))))


