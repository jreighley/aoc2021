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

(def datap2
  (let [data (->> data
               (map #(clojure.string/split % #" ")))]
    (for [line data]
      {:signal (group-by #(.length %) (take 10 line)) :output (take-last 4 line)})))



(defn compute-digit [facts digit]
  (let [novel-fact (cond (= 2 (.length digit)) {(set digit) 1}
                         (= 3 (.length digit)) {(set digit) 7}
                         (= 4 (.length digit)) {(set digit) 4}
                         (= 8 (.length digit)) {(set digit) 8})]
       (conj facts novel-fact)))



(comment (think {} '("af" "bafc" "fea" "af")))

