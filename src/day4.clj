(ns day4)

(def data-lines (->> (slurp "resources/day4")
                     (clojure.string/split-lines)
                     (remove #(= "" %))))

(def n-call-seq (mapv #(Long/parseLong %)(clojure.string/split (first data-lines) #",")))

(defn cols [card]
  (for [col (range 5)]
    (set (map #(nth  % col) card))))

(defn win-sets [card]
  (let [rows (map #(set %) card)
        cols  (cols card)]
    (flatten [ rows cols])))

(defn process-line [bcl]
  (->> bcl
       (partition-all 3)
       (map #(apply str %))
       (map clojure.string/trim)
       (map #(Long/parseLong %))))

(def bingo-cards
  (->> (rest data-lines)
       (map process-line)
       (partition 5)))

(defn call-order [n]
   (.indexOf n-call-seq n))

(defn find-win [win-sets]
  (reduce min (for [ws win-sets]
                (->> ws
                 (map call-order)
                 (reduce max)))))

(defn card-score [called-ns card]
   (->> card
        (flatten)
        (remove (set called-ns))
        (reduce +)
        (* (last called-ns))))

(defn find-win-info [card]
  (let [win-combos (win-sets card)
        earliest-win (find-win win-combos)
        called-numbers (take (inc earliest-win) n-call-seq)]
    {:win-turn earliest-win
     :card card
     :called-numbers called-numbers
     :score (card-score called-numbers card)}))

(def scored-cards (sort-by :win-turn (map #(find-win-info % ) bingo-cards)))

(def answer-1 (:score (first scored-cards))) ;67716
(def answer-2 (:score (last scored-cards)))  ;1830


