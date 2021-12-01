(ns day1)

(def data (->> (slurp "resources/day1")
               (clojure.string/split-lines)
               (map  #(Integer/parseInt %))))

(defn inclist
  ([acc m seq]
   (if ( empty? seq)
     acc
     (let [newm (first seq)
           newseq (rest seq)
           new-acc (when m
                     (if (< m newm)
                       (conj acc newm)
                       acc))]
       (recur new-acc newm newseq)))))

(def answer-1 (->> data
                   (inclist [] nil)
                   (count)))

(defn seq-sums [acc seq]
  (if (= 2  (count seq))
    acc
    (recur (conj acc (apply + (take 3 seq))) (rest seq))))

(def answer-2
  (->> data
       (seq-sums [])
       ( inclist [] nil)
       (count)))


