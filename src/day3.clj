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
        max-count (if (apply = (vals digit-freqs))
                      \1
                      (key (apply max-key val digit-freqs)))
        filtered-ds (filter #(= max-count (nth % idx))ds)]
    (println (str "maxount" max-count))
    filtered-ds))

(defn filter-least-common-bit [idx ds]
  (let [digit-freqs (frequencies (map #(nth % idx) ds))
        min-count (if (apply = (vals digit-freqs))
                    \0
                    (key (apply min-key val digit-freqs)))
        filtered-ds (filter #(= min-count (nth % idx)) ds)]
    (println (str "minount" min-count))
    (println filtered-ds)
    filtered-ds))

(defn reduce-ds [func idx ds]
  (if (or (= 1 count ds)
          (= idx 11))
    ds
    (do (println ds)
        (recur func (inc idx) (func idx ds)))))

(comment
  (reduce-ds filter-most-common-bit 0 data))

(comment
  (bin->int (str 1 1 1 0 0 0 0 0 0 0 1 0)) ; 3586
  (bin->int "001100010101"))  ;789 low

; still messy Need to figutre out how to fix the end conditions.
