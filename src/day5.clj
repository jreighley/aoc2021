(ns day5)

(defn read-line-coords [line]
  (->> (re-seq #"\d*" line)
    (remove empty?)
    (map #(Long/parseLong %))))

(def horizontal? #(= (nth  % 0) (nth % 2)))
(def vertical? #(= (nth  % 1) (nth % 3)))
(def lines (->> (slurp "resources/day5")
               (clojure.string/split-lines)
               (map read-line-coords)))

(defn line-points [line]
  (let [[x1 y1 x2 y2] line
        max-x (max x1 x2)
        min-x (min x1 x2)
        max-y (max y1 y2)
        min-y (min y1 y2)]
    (for [ys (range min-y (inc max-y))
          xs (range min-x (inc max-x))]
       [xs ys])))

(def x-lines (filter horizontal? lines))
(def y-lines (filter vertical? lines))
(def h-v-lines (into x-lines y-lines))

(def hv-line-points  (reduce into (map  line-points h-v-lines)))

(def answer-1 (->> hv-line-points
                   frequencies
                   (remove #(=  (val %) 1))
                   (count))) ;5124 correct
