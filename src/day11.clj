(ns day11)
(def input (slurp "resources/day11"))

(defn process-line [line]
  (->> line
       (partition 1)
       (map #(apply str %))
       (mapv #(Long/parseLong %))))

(def data (->>(slurp "resources/day11")
              (clojure.string/split-lines)
              (mapv process-line)))

(def legal-grid (apply sorted-set  (range 10)))

(def all-cells
  (for [r legal-grid
        c legal-grid]
    [r c]))

(defn neighbors [r c]
  (keep #(identity %)
        (for [row (keep legal-grid (range (- r 1) (+ r 2)))
              col (keep legal-grid (range (- c 1) (+ c 2)))]
          (if (= [r c] [row col]) nil [row col]))))

(defn add-energy [board r c]
  (update board r
    #(update % c inc)))

(def exploding? #(< 9 %))
(def negitivize (partial * -1))



(def all-cells
  (for [r legal-grid
        c legal-grid]
    [r c]))

(defn update-all [board cell-list]
  (if (empty? cell-list)
    board
    (recur (apply add-energy board (first cell-list)) (rest cell-list))))

(defn explode! [board r c]
  (let [imploded-board (update board r
                               #(update % c negitivize))]
    (update-all imploded-board (neighbors r c))))

(comment (update-all data all-cells))