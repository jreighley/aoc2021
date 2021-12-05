

(ns day4)

(def data-lines (->> (slurp "resources/day4")
                     (clojure.string/split-lines)
                     (remove #(= "" %))))

(def numbers-call-list (map #(Long/parseLong %)(clojure.string/split (first data-lines) #",")))

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

(def card-win-sets  (map win-sets bingo-cards))

(defn winner? [nlist card-pos]  (clojure.set/superset? (set nlist) card-pos))

(defn anywinner? [nlist] (remove empty? (for [card card-win-sets]
                                            (if ((complement empty?) (filter #(winner? nlist %) card))
                                              card))))

(defn calc-sum-unmaprked [wc]
  (->> wc
       (first)
       (reduce into #{})
       (remove (set (take 22 numbers-call-list)))
       (remove #(= % ""))
       (map #(Long/parseLong %))
       (reduce +)))

(def winning-card (->> (take 22 numbers-call-list)
                       (anywinner?)))
(def call-sets (for [n (range 5 (count  numbers-call-list))]
                 (set  (take n numbers-call-list))))

(defn card-info [card]
  (let [card-win-cond (win-sets card)]

      card-win-cond))

(defn checkwin? [win-conds called-list]
   (filter #(clojure.set/superset? called-list %) win-conds))

(comment
     (for [call call-sets]
          (let [wins (seq (checkwin? (card-info (first bingo-cards)) call))]

            ( wins))))


(comment (calc-sum-unmaprked winning-card)) ;; 1026
;; )


n
(comment "66 is the number"
  win-sets)