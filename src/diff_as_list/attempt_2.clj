(ns diff-as-list.attempt-2
  (:require [clojure.test :as test]
            [clojure.set :as _set]))

(defn- is-primitive? [val]
  (contains? #{"java.lang.String" "java.lang.Long" "clojure.lang.Keyword"} (.getName (type val))))

;; (deftest primitive
;;   (is (= true (is-primitive? "test")))
;;   (is (is-primitive? 1))
;;   (is (not (is-primitive? {})))
;;   (is (not (is-primitive? []))))


(defn diffl
  ([arg-1 arg-2] (diffl arg-1 arg-2 [] [] []))
  ([arg-1 arg-2 keypath keys-to-traverse result]
   (if (or (nil? arg-1) (nil? arg-2))
     (if (and (nil? arg-1) (nil? arg-2))
       result
       (conj result {:keypath keypath
                     :value-1 arg-1
                     :value-2 arg-2}))
     (let [arg-1-type (.getName (type arg-1))
           arg-2-type (.getName (type arg-2))]
       (if-not (= arg-1-type arg-2-type)
         (conj result {:keypath keypath
                       :typediff {:arg-1-type arg-1-type
                                  :arg-2-type arg-2-type}})
         (cond
           (is-primitive? arg-1)
           (if (= arg-1 arg-2)
             result
             (conj result {:keypath keypath
                           :value-1 arg-1
                           :value-2 arg-2}))
           (map? arg-1)
           (if (> (count keys-to-traverse) 0)
             (let [next-key (first keys-to-traverse)]
               (recur (get arg-1 next-key)
                      (get arg-2 next-key)
                      (conj keypath next-key)
                      (rest keys-to-traverse)
                      result))
             (let [keys-in-1 (set (keys arg-1))
                   keys-in-2 (set (keys arg-2))
                   keys-missing-in-2 (_set/difference keys-in-1 keys-in-2)
                   keys-missing-in-1 (_set/difference keys-in-2 keys-in-1)
                   keys-in-both (_set/intersection keys-in-1 keys-in-2)
                   new-result (if (and (= 0 (count keys-missing-in-1))
                                       (= 0 (count keys-missing-in-2)))
                                result
                                (conj result {:keypath keypath
                                              :keys-missing-in-1 keys-missing-in-1
                                              :keys-missing-in-2 keys-missing-in-2}))]
               (recur arg-1 arg-2 keypath keys-in-both new-result)))
           :else
           (throw (Exception. (str "don't know how to handle type " (.getName (type arg-1)))))))))))


(test/deftest easy
  (test/is (= [] (diffl 1 1)))
  (test/is (= [] (diffl nil nil)))
  (test/is (= [{:keypath [], :value-1 nil, :value-2 1}] (diffl nil 1)))
  (test/is (= [{:keypath [], :value-1 1, :value-2 nil}] (diffl 1 nil)))
  (test/is (= [{:keypath [], :value-1 1, :value-2 2}] (diffl 1 2))))

(test/deftest nesting
  (test/is (= [] (diffl {:one 1} {:one 1})))
  (test/is (= [{:keypath [:one], :value-1 1, :value-2 2}] (diffl {:one 1} {:one 2})))
  (let [result (diffl {:one {:two {:three 4}}} {:one {:two {:three 5}}})]
    (test/is (= [{:keypath [:one :two :three], :value-1 4, :value-2 5}] result ))))


(test/deftest keys-missing
  (let [result (diffl {:one 1 :two 2} {:one 1})]
    (test/is (= [{:keypath [], :keys-missing-in-1 #{}, :keys-missing-in-2 #{:two}}] result))))




;; F. Henard 5/19/18 - stopped here - this is hard
(test/deftest keys-missing-multiple
  (let [expected [{:keypath [:one], :value-1 1, :value-2 2}
                  {:keypath [:two], :value-1 3, :value-2 4}]
        result (diffl {:one 1 :two 2} {:one 3 :two 4})]
    (test/is (= expected result))))
