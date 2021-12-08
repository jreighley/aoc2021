(ns day8
 (:require [ clojure.set :as cs]))

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

(defn set->str [k]
  (->> k
       sort
       (apply str)))

(defn calc-line-decryter [{:keys [signal output]}]
      (let  [one (set (first  (get signal 2)))
             four (set (first  (get signal 4)))
             seven (set (first  (get signal 3)))
             eight (set (first  (get signal 7)))
             five-segs (map set  (get signal 5))
             six-segs (map set  (get signal 6))
             nine (first  (filter #(cs/subset?  four %) six-segs))
             top-seg (cs/difference seven one)
             three (first  (filter #(cs/subset?  seven %) five-segs))
             upper-left-seg (cs/difference four three)
             five (first (filter #(cs/subset? upper-left-seg  %) five-segs))
             two (first  (remove #{three five} five-segs))
             upper-right (cs/difference nine five)
             six (set (remove upper-right eight))
             zero (first (remove #{six nine } six-segs))

             decrypter {(set->str one) 1
                        (set->str two) 2
                        (set->str three) 3
                        (set->str four) 4
                        (set->str five) 5
                        (set->str six) 6
                        (set->str seven) 7
                        (set->str eight) 8
                        (set->str nine) 9
                        (set->str zero) 0}
             sorted-output (map #(-> %
                                     set
                                     set->str)output)
             output-digits (map decrypter sorted-output)]
        (Long/parseLong (apply str output-digits))))

(defn calc-answer-2 [input]
  (reduce + (map calc-line-decryter input)))

(calc-answer-2 datap2) ;1016804 correct

(comment  (->> (get  (:signal  (first datap2))6)))

(comment (calc-line-decryter (first datap2)))

