(ns day8)

(def data (->> (slurp "resources/day8")
               (clojure.string/split-lines)))

(defn count-segments [output]
  (->> output
       (filter #(#{2 3 4 7} (.length %)))))

(defn calculate-answer-1 [input]
  (->> input
       (map #(clojure.string/split % #" "))
       (map #(take-last 4 %))
       (map count-segments)
       (map count)
       (reduce +)))

(calculate-answer-1 data)  ;521 is correct

(->> data
  (map #(clojure.string/split % #" "))
  (map #(take-last 4 %)))

(defn guess-digit [facts digit]
  (cond (= 2 (.length digit)) {(set digit) 1}
        (= 3 (.length digit)) {(set digit) 7}
        (= 4 (.length digit)) {(set digit) 4}
        (= 8 (.length digit)) {(set digit) 8}))

(defn think [facts puzzles]
  (if empty? puzzles
    facts))



(defn deduce-numbers [output-list]
  (let [computables (count-segments output-list)
        ones (-> (filter #(= 2 (.length %)) output-list))
        fours (filter #(= 4 (.length %)) output-list)
        sevens (filter #(= 3 (.length %)))
        eights (filter #(= 8 (.length %)))]
    computables))

(deduce-numbers '("ab" "bdacfeg" "ab" "bga"))