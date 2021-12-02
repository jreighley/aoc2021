(ns day1
  (:require [utils :refer :all]))

(def data (->> (slurp "resources/day1")
               (clojure.string/split-lines)
               (map  #(Integer/parseInt %))))
(def data1 (read-number-file "resources/day1"))

(defn accending
  "returns a new measure mark and an accending number if there is one"
  [old-measure new-measure]
  (if (or ( nil? old-measure)
          (< new-measure old-measure))
    [new-measure nil]
    [new-measure new-measure]))

(defn inclist
  "Returns a list of numbers that are accending from a seq"
  [acc m seq]
  (if ( empty? seq)
    acc
    (let [[newm new-accender ] (accending m (first seq))
          newseq (rest seq)
          new-acc (if new-accender
                    (conj acc new-accender)
                    acc)]
      (recur new-acc newm newseq))))

(def answer-1 (->> data
                   (inclist [] nil)
                   (count)))
;; 1448

(defn seq-sums [acc seq]
  (if (= 2  (count seq))
    acc
    (recur (conj acc (apply + (take 3 seq))) (rest seq))))

(def answer-2
  (->> data
       (seq-sums [])
       ( inclist [] nil)
       (count)))
; 1471


;; Better solution:
(defn count-accentions [input]
  (->> input
       (partition 2 1)
       (filter #( < (first %) (last %)))
       (count)))

(defn triple-sums [seq]
  (->> seq
       (partition 3 1)))

(defn count-triple-sums-accending [data]
  (->> (triple-sums data1)
       (map #(apply + %))
       (count-accentions)))

(comment
  "part 1" 1448
  (count-accentions data1))

(comment
  "part 2" 1471
  (count-triple-sums-accending data))

