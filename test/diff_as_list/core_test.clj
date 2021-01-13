(ns diff-as-list.core-test
  (:require [clojure.test :as t]
            [diff-as-list.core :as ut]))

(t/deftest scalar
  (t/is (= true (ut/is-scalar? "test")))
  (t/is (ut/is-scalar? 1))
  (t/is (not (ut/is-scalar? {})))
  (t/is (not (ut/is-scalar? []))))


(t/deftest ttf-1
  (let [actual (ut/traverse-to-flat {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                                          :level-3-2 {:level-4-1 "level-4-1-val"}}
                                              :level-2-2 "level-2-2-val"
                                              :level-2-3 "level-2-3-val"}
                                  :level-1-2 "level-1-2-val"})
        expected {[] {:type :diff-as-list.core/map},
                  [:level-1-1] {:type :diff-as-list.core/map},
                  [:level-1-1 :level-2-1] {:type :diff-as-list.core/map},
                  [:level-1-1 :level-2-1 :level-3-1]
                  {:type :diff-as-list.core/scalar, :value "level-3-1-val"},
                  [:level-1-2]
                  {:type :diff-as-list.core/scalar, :value "level-1-2-val"},
                  [:level-1-1 :level-2-2]
                  {:type :diff-as-list.core/scalar, :value "level-2-2-val"},
                  [:level-1-1 :level-2-3]
                  {:type :diff-as-list.core/scalar, :value "level-2-3-val"},
                  [:level-1-1 :level-2-1 :level-3-2] {:type :diff-as-list.core/map},
                  [:level-1-1 :level-2-1 :level-3-2 :level-4-1]
                  {:type :diff-as-list.core/scalar, :value "level-4-1-val"}}]
    ;; (pp/pprint actual)
    (t/is (= actual expected))))

(t/deftest ttf-2
  (let [actual (ut/traverse-to-flat  {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                                           :level-3-2 nil
                                                           }
                                               :level-2-2 "level-2-2-val"
                                               :level-2-3 "level-2-3-val"}
                                   :level-1-2 "level-1-2-val"})
        expected {[] {:type :diff-as-list.core/map},
                  [:level-1-1] {:type :diff-as-list.core/map},
                  [:level-1-1 :level-2-1] {:type :diff-as-list.core/map},
                  [:level-1-1 :level-2-1 :level-3-1]
                  {:type :diff-as-list.core/scalar, :value "level-3-1-val"},
                  [:level-1-2]
                  {:type :diff-as-list.core/scalar, :value "level-1-2-val"},
                  [:level-1-1 :level-2-2]
                  {:type :diff-as-list.core/scalar, :value "level-2-2-val"},
                  [:level-1-1 :level-2-3]
                  {:type :diff-as-list.core/scalar, :value "level-2-3-val"},
                  [:level-1-1 :level-2-1 :level-3-2]
                  {:type :diff-as-list.core/scalar, :value nil}}]
    ;; (pp/pprint actual)
    (t/is (= actual expected))))



(t/deftest initial
  (let [expected {:differences [(ut/make-value-diff
                                 {:path [:what],
                                  :val-1 "nothing",
                                  :val-2 "who"})]}
        actual (-> (ut/diffl {:what "nothing"} {:what "who"})
                   (dissoc :dal-version))]
    (t/is (= expected actual))))


(t/deftest nil-arg
  (let [expected {:differences [(ut/make-value-diff
                                 {:path [:what],
                                  :val-1 nil,
                                  :val-2 {:who "why"}})]}
        actual (-> (ut/diffl {:what nil} {:what {:who "why"}})
                   (dissoc :dal-version))]
    (t/is (= expected actual))))

(t/deftest missing-key
  (let [map-1 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"
                                       :level-3-2 {:level-4-1 "level-4-1-val"}}
                           :level-2-2 "level-2-2-val"
                           :level-2-3 "level-2-3-val"}
               :level-1-2 "level-1-2-val"}
        map-2 {:level-1-1 {:level-2-1 {:level-3-1 "level-3-1-val"}
                             :level-2-2 "level-2-2-val"
                             :level-2-3 "level-2-3-val"}
               :level-1-2 "level-1-2-val"}
        expected {:differences [(ut/make-key-missing-diff
                                 {:path [:level-1-1 :level-2-1 :level-3-2]
                                  :value {:level-4-1 "level-4-1-val"}
                                  :missing-in :two})]}
        actual (-> (ut/diffl map-1 map-2)
                   (dissoc :dal-version))]
    ;; (pp/pprint actual)
    (t/is (= expected actual))))

(t/deftest remove-redundant
  (let [map-1 {:question-section
               {:name "question-section",
                :fields {:text {:name "text", :type :character, :max-length 200}}}}
        map-2 {:question-section
               {:name "question-section",
                :fields
                {:text {:name "text", :type :character, :max-length 200},
                 :bogus {:name "bogus", :type :integer},
                 :what {:name "what", :type :djunk}}}}
        expected {:differences
                  [(ut/make-key-missing-diff
                    {:path [:question-section :fields :bogus],
                     :value {:name "bogus", :type :integer}
                     :missing-in :one})
                   (ut/make-key-missing-diff
                    {:path [:question-section :fields :what],
                     :value {:name "what", :type :djunk}
                     :missing-in :one})]}
        actual (-> (ut/diffl map-1 map-2)
                   (dissoc :dal-version))]
    ;; (println "\nexpected:")
    ;; (pp/pprint expected)
    ;; (println "\n:actual:")
    ;; (pp/pprint actual)
    (t/is (= expected actual))))






(t/deftest patch-initial
  (let [orig-map {:what "nothing"}
        _diff {:differences [(ut/make-value-diff
                              {:path [:what],
                               :val-1 "nothing",
                               :val-2 "who"})]}
        expected {:what "who"}
        actual (ut/patch orig-map _diff)]
    ;; (pp/pprint actual)
    (t/is (= expected actual))))

(t/deftest patch-nil-arg
  (let [orig-map {:what nil}
        _diff {:differences [(ut/make-value-diff
                              {:path [:what],
                               :val-1 nil,
                               :val-2 {:who "why"}})]}
        expected {:what {:who "why"}}
        actual (ut/patch orig-map _diff)]
    (t/is (= expected actual))))


(t/deftest patch-key-removed
  (let [orig-map {:what "one"
                  :who "two"}
        _diff {:differences [(ut/make-key-missing-diff
                              {:path [:who]
                               :value "two"
                               :missing-in :two})]}
        expected {:what "one"}
        actual (ut/patch orig-map _diff)]
    (t/is (= expected actual))))

(t/deftest patch-key-removed-nested
  (let [orig-map {:what {:why "who"}}
        _diff {:differences [(ut/make-key-missing-diff
                              {:path [:what :why]
                               :value "who"
                               :missing-in :two})]}
        expected {:what {}}
        actual (ut/patch orig-map _diff)]
    (t/is (= expected actual))))


(t/deftest patch-key-added
  (let [orig-map {:what {}}
        _diff {:differences [(ut/make-key-missing-diff
                              {:path [:what :why]
                               :value "who"
                               :missing-in :one})]}
        expected {:what {:why "who"}}
        actual (ut/patch orig-map _diff)]
    (t/is (= expected actual))))


(t/deftest patch-nil-to-value
  (let [orig-map nil
        _diff {:differences [(ut/make-value-diff
                              {:path [],
                               :val-1 nil,
                               :val-2 {:what "who"}})]}
        expected {:what "who"}
        actual (ut/patch orig-map _diff)]
    (t/is (= expected actual))))



(comment

  {:differences [(ut/make-key-missing-diff
                  {:path [:what :why]
                   :value "who"
                   :missing-in :one})
                 (ut/make-value-diff
                  {:path [],
                   :val-1 nil,
                   :val-2 {:what "who"}})]}


  )
