(ns day12)

(def input (->> (slurp "resources/day12")
                (clojure.string/split-lines)))

(def tunnels (map #(set (clojure.string/split % #"-")) input))

(defn node-options [node]
  (remove #{node }(->> tunnels
                   (filter #(contains? % node))
                   (reduce into))))

(defn small? [node]
  (= node (.toLowerCase node)))

(defn explored? [visited]
   (->> visited
     (filter small?)
     (set)))

(defn find-next-node [path]
  (->> (node-options (last path))
       (remove (explored? path))
       (map #(conj path %))))

(defn find-all-paths [finished v-incompletes]
  (let [new-paths  (reduce into [] (mapv #(find-next-node % ) v-incompletes))
        new-finished (into finished (filter #(= "end" (last %))new-paths))]
    (if ( empty? v-incompletes)
      (count new-finished)
      (do (recur new-finished new-paths)))))

(defn find-next-node2  [path]
   (let [visit-max-freq (apply max  (vals (frequencies  (filter small? path))))
         set2remove (if (= 2 visit-max-freq)
                        (explored? path)
                        #{"start"})]
     (->> (node-options (last path))
          (remove set2remove)
          (map #(conj path %)))))

(defn find-all-paths2 [finished v-incompletes]
  (let [new-paths  (->> (reduce into [] (mapv #(find-next-node2 % ) v-incompletes)))
        new-finished (into finished (filter #(= "end" (last %))new-paths))]
    (if ( empty? v-incompletes)
      (count new-finished)
      (do (recur new-finished (->> new-paths
                                (remove #(= "end" (last %)))))))))

(comment
   (find-all-paths [] [["start"]])) ;;;4970 correct

(comment
  (find-all-paths2 [] [["start"]])) ;137948 correct




