(ns day9)

(defn process-line [line]
  (->> line
       (partition 1)
       (map #(apply str %))
       (mapv #(Long/parseLong %))))

(def data (->>(slurp "resources/day9")
              (clojure.string/split-lines)
              (mapv process-line)))

(apply min (first (take 2 data)))




(defn get-digits [sn]
 (->> sn
      (partition 1)
      (map str)
      (map #(Long/parseLong %))))


(def data (->> (slurp "resources/day9")
               (re-seq #"\d")
               (mapv #(Long/parseLong %))))

(defn altitude [topo point]
  (nth topo point))

(defn neighbors [n]
  (->>  (case (rem n 100)
          0 [ (- n 100) (+ n 100) (+ n 1)]
          99 [ (- n 100) (+ n 100) (- n 1)]
          [ (- n 100) (+ n 100) (+ n 1) (- n 1)])
    (remove neg?)
    (remove #(<  9999 %))))

(defn score-location [topo point]
  (let [ neighbor-points (neighbors point)
        neighbor-alts (map #(altitude topo %) neighbor-points)
        pt-alt (altitude topo point)]
    (if (< pt-alt (apply min neighbor-alts))
      (inc pt-alt))))

(comment answer-1
   (reduce + (keep identity (map #(score-location data %) (range (count data))))))  ;537 correct high

(defn low-points [topo point]
  (let [ neighbor-points (neighbors point)
        neighbor-alts (map #(altitude topo %) neighbor-points)
        pt-alt (altitude topo point)]
    (if (< pt-alt (apply min neighbor-alts))
      point)))

(defn uphill-neighbors
  ([topo point]
   (let [pa (altitude topo point)
         np (neighbors point)
         na (for [npoint np]
              [npoint (altitude topo npoint)])
         uphill-neighbors (->> na
                               (filter  #(< pa (second %)))
                               (filter #(< (second  %)  9)))]
     (mapv first uphill-neighbors))))

(defn build-basin [topo explored-points frontier-points]
  (if (empty? frontier-points)
      explored-points
      (let [exploring (first frontier-points)
            remaining-frontier (rest frontier-points)
            novel-uphill (->> (uphill-neighbors topo exploring)
                           (remove explored-points))
            new-frontier  (into remaining-frontier novel-uphill)
            new-explored (conj explored-points exploring)]
        (recur topo new-explored new-frontier))))

(def basin-bottoms (keep identity (map #( low-points data %)  (range 10000))))

(comment  "answer-2"
  (reduce * (map count (take-last 3 (sort-by count (pmap #(build-basin data #{} [%]) basin-bottoms))))))

;1142757 is correct