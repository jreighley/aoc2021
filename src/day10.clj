(ns day10)

(def data (slurp "resources/day10"))

(def testdata (seq "{([(<{}[<>[]}>{[]{[(<()>"))

(def expected-closer {\( \)
                      \[ \]
                      \{ \}
                      \< \>})

(def opener? #{\( \[ \{ \<})
(def closer? #{\) \] \{ \>})

(defn process-input-line
  ([l]
   (let [line (seq l)
         first-char (take 1 line)
         ec (list (get expected-closer (char first-char)))]
     (do (println ec)
         (process-input-line ec (rest line)))))
  ([exp line]
   (println exp)
   (if (empty? line)
     nil
     (let [first-char (take 1 line)
           expected-close? (= (peek exp) first-char)
           new-exp (if (opener? first-char)
                     (conj (get expected-closer first-char) exp)
                     (when expected-closer
                       (pop exp)))]
          (if (not (or (opener? first-char)
                       (false? expected-close?)))
             first-char
             (do (println exp)
                 (recur new-exp (rest line))))))))



(comment (process-input-line testdata))
(take-while opener? testdata)
