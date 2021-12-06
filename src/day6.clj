(ns day6
  (:require [utils :refer :all]))


(def init-fish-list (map #(Long/parseLong %)
                         (-> (slurp "resources/day6")
                             (clojure.string/split  #","))))

(def fish-pop-by-cycle-time (frequencies init-fish-list))

(defn schedule-spawn [generation poplist poplimit]
  (if (= generation poplimit)
    (reduce + (vals poplist))
    (let [spawners (get poplist generation)
          respawn-day (+ generation 7)
          baby-respawn-day (+ generation 9)
          scheduled-spawns (get poplist respawn-day)
          scheduled-baby-spawns (get poplist baby-respawn-day)
          new-spawns {respawn-day (+' (or spawners 0 )(or scheduled-spawns 0))
                      baby-respawn-day (+' (or spawners 0) (or scheduled-baby-spawns 0))}]
     (recur (inc generation)
            (-> poplist
                (conj new-spawns)
                (dissoc generation))
            poplimit))))

(comment answer-1
         (schedule-spawn 1 fish-pop-by-cycle-time 80)
         => 362346)
(comment answer-2
         (schedule-spawn 1 fish-pop-by-cycle-time 256)
         => 1639643057051)


