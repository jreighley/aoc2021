(ns day2
  (:require [utils :refer :all]))



(def data (->> (slurp "resources/day2")
               (clojure.string/split-lines)
               (map #(clojure.string/split % #" "))))

(def ups (->> data
              (filter #(= "up" (first %)))
              (map last)
              (map #(Long/parseLong %))
              (reduce +)))
(def downs (->> data
                (filter #(= "down" (first %)))
                (map last)
                (map #(Long/parseLong %))
                (reduce +)))
(def forwards (->> data
                   (filter #(= "forward" (first %)))
                   (map last)
                   (map #(Long/parseLong %))
                   (reduce +)))

(def depth (-  downs ups))
(comment (* forwards depth)) ;1604850 correct
(defn adjust-aim)
(defn find-position
  ([command-stack]
   (find-position [0 0] 0 command-stack))
  ([location aim command-stack]
   (if (empty? command-stack)
       location
       (let [[distance depth] location
             newcs (rest command-stack)
             [cmd n] (->> command-stack
                          (first))]))))






