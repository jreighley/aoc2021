(ns day7)

(def crab-locs (map #(Long/parseLong %)
                    (-> (slurp "resources/day7")
                      (clojure.string/split #","))))

(defn abs [n]
  (if (neg? n)
      (* -1 n)
      n))

(defn fuel-needed2loc [loc-n]
      (->> crab-locs
           (map #(- loc-n %))
           (map abs)
           (reduce +)))

(def answer-1 (apply min (map fuel-needed2loc (range 1800)))) ;344297 correct

(defn distance->fuel [d]
  (reduce + (map inc (range d))))

(defn fuel-needed2locv2 [loc-n]
  (->> crab-locs
       (map #(- loc-n %))
       (map abs)
       (map distance->fuel)
       (reduce +')))

(def answer-2 (apply min (pmap fuel-needed2locv2 (range 1800))))  ;97164301 correct