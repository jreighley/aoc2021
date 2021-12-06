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

(def call-sets (for [n (range 1 (count  numbers-call-list))]
                 (set  (take n numbers-call-list))))

(defn card-info [card]
  (let [card-win-cond (win-sets card)]
      card-win-cond))

(defn shortest-superset [set]
  (let [called-digits (->> call-sets
                           (filter #(clojure.set/superset? % set))
                           (sort-by #(count %))
                           (first))
        n-digits (count called-digits)]
    {:called called-digits :number  n-digits}))

(defn find-winnning-call [card]
  (let [winning-sets (->> card
                          (win-sets)
                          (map shortest-superset)
                          (sort-by :number)
                          (first))]
    (assoc winning-sets
      :card card)))

(def losing-card (->> (map find-winnning-call bingo-cards)
                      (filter #(pos? (:number %)))
                      (sort-by :number)
                      (last)))

(def winning-card (->> (map find-winnning-call bingo-cards)
                       (filter #(pos? (:number %)))
                       (sort-by :number)
                       (first)))

(* (->> winning-card
        (:card)
        (reduce into)
        (set)
        (remove :called)
        (reduce +))

  (->> winning-card
       :number
       dec
       (nth numbers-call-list)))  ;should be 67716

;65 wrong -- probalby off by one?
;89895 too high  12447 also too high..

(keep (set (->> losing-card
                :called)) numbers-call-list) ;9




(defn checkwin? [win-conds called-list]
   (filter #(clojure.set/superset? called-list %) win-conds))
(def bingo? #((complement empty?) %))


(comment "66 is the number"
  win-sets)