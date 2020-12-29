(ns nsu-labs.core-test
    (:require [clojure.test :refer :all]
              [nsu-labs.core :refer :all]))


(deftest filter-tests
  (is
   (=
    (filter my-wierd-predicate (range 1000000))
    (parallel-filter my-wierd-predicate (range 1000000))))
  (is
   (=
    (parallel-filter (fn [junk] true) (range 1000000))
    (range 1000000)))
  (is
   (=
    (parallel-filter (fn [junk] false) (range 1000000))
    '()))
  (is
   (= (take 9 (filter prime? (iterate inc 1)))
      (take 9 (parallel-filter prime? (iterate inc 1))))))