(ns day10)

(def data (clojure.string/split-lines (slurp "resources/day10")))

(def expected-closer {\( \)
                      \[ \]
                      \{ \}
                      \< \>})

(def opener? #{\( \[ \{ \<})

(def score-paren { \) 3
                  \] 57
                  \} 1197
                  \> 25137})

(defn find-mismatch
  ([p-str]
   (find-mismatch (conj '() (expected-closer (first p-str))) (rest (seq p-str))))
  ([exp-c p-seq]
   (let [paren (first p-seq) new-exp-paren (get expected-closer paren)
         expected? (and (nil? new-exp-paren)
                        (= (first exp-c) paren))
         new-exp   (if (opener? paren)
                       (conj exp-c new-exp-paren)
                     (pop exp-c))
         corrupt-paren? (not (or (opener? paren)
                                 expected?))]
     ;(println (str "corupt?" corrupt-paren?  " new expect" new-exp "expected? " expected?))
     (if corrupt-paren?
         (get score-paren paren)
         (when (rest p-seq)
           (recur new-exp (rest p-seq)))))))


(comment "answer-1 " (reduce + (keep identity (map find-mismatch data))))

(def close-paren-score {\) 1
                        \] 2
                        \} 3
                        \> 4})

(defn score-autocomplete [ paren-seq]
  (loop [score-stack (map close-paren-score paren-seq)
         acc  0]
    (if (empty? score-stack)
      acc
      (recur (rest score-stack) (+ (* 5 acc) (first score-stack))))))

(defn find-incomplete
  ([p-str]
   (find-incomplete (conj '() (expected-closer (first p-str))) (rest (seq p-str))))
  ([exp-c p-seq]
   (let [paren (first p-seq)
         new-exp-paren (get expected-closer paren)
         expected? (and (nil? new-exp-paren)
                        (= (first exp-c) paren))
         new-exp   (if (opener? paren)
                     (conj exp-c new-exp-paren)
                     (pop exp-c))
         corrupt-paren? (not (or (opener? paren)
                                 expected?))]
     ;(println (str "corupt?" corrupt-paren?  " new expect" new-exp "expected? " expected?))
     (if corrupt-paren?
       nil
       (if (empty? (rest p-seq))
         (score-autocomplete new-exp)
         (recur new-exp (rest p-seq)))))))

(defn median [vallist]
  (nth (sort vallist) (Math/floor (/  (count vallist) 2.0))))

(comment answer-2 (median (keep identity (map find-incomplete data))))
