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

(def exploding? #(< 9 %))
(def negitivize (partial * -2))
(def zeroize (partial * -1))

(def update-ops {:add inc
                 :neg negitivize
                 :zero zeroize})

(defn update-cell [board op r c]
  (update board r
    #(update % c (op update-ops))))


(defn get-rv [board r c]
  (nth (nth board r) c))

(def all-cells
  (for [r legal-grid
        c legal-grid]
    [r c]))

(defn find-exploding [board]
  (for [[r c] all-cells
        :when (exploding? (get-rv board r c))]
    [r c]))
(declare update-cells)

(defn explode! [board r c]
  (let [imploded-board (update board r
                               #(update %  r c :negitivize))]
    (update-cells imploded-board :add  (neighbors r c))))

(defn chain-react [board]
  (let [exploding-cells (find-exploding board)
        new-board (update-cells board :negativize exploding-cells)]
    new-board))


(defn update-cells [board op cell-list]
  (if (empty? cell-list)
    board
    (recur  (->> (first cell-list)
                 (apply update-cell board op))
                 ;(chain-react))
            op
            (rest cell-list))))

(comment (update-cells data :add all-cells))

(comment (-> (update-cells data :add all-cells)
             (update-cells   all-cells)))




