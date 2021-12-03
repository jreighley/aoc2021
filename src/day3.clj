(ns day3)

(def data  (->> (slurp "resources/day3")
               (clojure.string/split-lines)
               (map seq)))

(defn bin->int [bits]
  (Integer/parseInt bits 2))

(defn find-gamma [ds]
  (apply str (for [idx (range (count (first ds)))]
               (key (apply max-key  val (frequencies  (val (first {idx  (->> ds
                                                                          (map #(nth % idx)))}))))))))
(defn find-epsilon [ds]
  (apply str (for [idx (range (count (first ds)))]
               (key (apply min-key val (frequencies  (val (first {idx  (->> ds
                                                                            (map #(nth % idx)))}))))))))
(comment
  "day 1"
  (* (bin->int (find-gamma data)) ;"000110001010" 394
     (bin->int (find-epsilon data)))) ;"111001110101" 3701
  ;1458194 is correct)

(defn filter-most-common-bit [idx ds]
  (let [digit-freqs (frequencies (map #(nth % idx) ds))
        max-count (if (and (< 1 (count (vals digit-freqs)))
                           (apply = (vals digit-freqs)))
                      \1
                      (key (apply max-key val digit-freqs)))
        filtered-ds (filter #(= max-count (nth % idx))ds)]
    filtered-ds))

(defn filter-least-common-bit [idx ds]
  (let [digit-freqs (frequencies (map #(nth % idx) ds))
        min-count (if (and (< 1 (count (vals digit-freqs)))
                           (apply = (vals digit-freqs)))
                    \0
                    (key (apply min-key val digit-freqs)))
        filtered-ds  (filter #(= min-count (nth % idx)) ds)]
    filtered-ds))

(defn reduce-ds [func idx ds]
  (if  (= 1 (count ds))
    ds
    (recur func (inc idx) (func idx ds))))


(comment "solution 2 2829354"
         (* (bin->int (apply str (first (reduce-ds filter-least-common-bit 0 data))))
            (bin->int (apply str (first (reduce-ds filter-most-common-bit 0 data))))))