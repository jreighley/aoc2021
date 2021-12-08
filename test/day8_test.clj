(ns day8-test
  (:require [clojure.test :refer :all])
  (:require [day8 :refer :all]))

(def testdata (->> (slurp "resources/day8test")
                   (clojure.string/split-lines)))

(deftest segment-count
   (is (=  (calculate-answer-1 testdata) 26)))


