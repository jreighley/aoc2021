(ns day5)

(defn read-line-coords [line]
  (->> (re-seq #"\d*" line)
    (remove empty?)
    (map #(Long/parseLong %))))

(def horizontal? #(= (nth  % 0) (nth % 2)))
(def vertical? #(= (nth  % 1) (nth % 3)))
(def diagnal? #(not (or (horizontal? %)
                        (vertical? %))))

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
(def d-lines (filter diagnal? lines))
(def h-v-lines (into x-lines y-lines))

(def hv-line-points  (reduce into (map  line-points h-v-lines)))

(def answer-1 (->> hv-line-points
                   frequencies
                   (remove #(=  (val %) 1))
                   (count))) ;5124 correct
(defn diag-line-points [line]
  (let [[x1 y1 x2 y2] line
        max-x (max x1 x2)
        min-x (min x1 x2)
        max-y (max y1 y2)
        min-y (min y1 y2)
        x-dir (if (< x1 x2)
                <
                >)
        y-dir (if (< y1 y2)
                 <
                 >)
        xseq (sort x-dir (range min-x (inc max-x)))
        yseq (sort y-dir (range min-y (inc max-y)))]
   (sort (zipmap xseq yseq))))

(def diag-line-points (map diag-line-points d-lines))
(def all-line-points (reduce into hv-line-points diag-line-points))

(def answer-2 (->> all-line-points
                   frequencies
                   (remove #(=  (val %) 1))
                   (count))) ;19771
