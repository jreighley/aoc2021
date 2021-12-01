(ns day1-test
  (:require [clojure.test :refer :all])
  (:require [day1 :refer [inclist seq-sums]]))


(def example-data [199
                   200
                   208
                   210
                   200
                   207
                   240
                   269
                   260
                   263])

(deftest inclist-test
  (is (=  (->> example-data
               (inclist [] nil)
               (count))
          7)))

(deftest example-2
  (is (= (->> example-data
              (seq-sums [])
              (inclist [] nil)
              (count)))
      5))

