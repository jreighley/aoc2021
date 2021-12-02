(ns day2
  (:require [utils :refer :all]))

(defn as-kw-long [ [command magnitude]]
  [(keyword command) (Long/parseLong magnitude)])

(def data (->> (slurp "resources/day2")
               (clojure.string/split-lines)
               (map #(clojure.string/split % #" "))
               (map as-kw-long)))

(def init-sub-state {:depth 0 :distance 0 :aim 0})

(defn mutate-sub-state1 [sub-state [mutation magnitude]]
  (cond
    (= :up mutation) (update-in sub-state [:depth] - magnitude)
    (=  :down mutation) (update-in sub-state [:depth] + magnitude)
    (= :forward mutation) (update-in sub-state [:distance] + magnitude)))

(defn mutate-sub-state2 [sub-state [mutation magnitude]]
    (cond
      (= :up mutation) (update-in sub-state [:aim] - magnitude)
      (=  :down mutation) (update-in sub-state [:aim] + magnitude)
      (= :forward mutation) (-> sub-state
                                       (update-in  [:depth] +  (* (:aim sub-state)  magnitude))
                                       (update-in [:distance] + magnitude))))

(defn multiply-depth-distance [m]
  (* (:depth m) (:distance m)))

(defn find-final-sub-state1 [sub-state command-list]
  (if ( empty? command-list)
    sub-state
    (recur (mutate-sub-state1 sub-state (first command-list)) (rest command-list))))

(defn find-final-sub-state2 [sub-state command-list]
  (if (empty? command-list)
    sub-state
    (recur (mutate-sub-state2 sub-state (first command-list)) (rest command-list))))

(comment "part 1" (->> (find-final-sub-state1 init-sub-state data)
                       (multiply-depth-distance))) ;1604850

(comment "part 1" (->> (find-final-sub-state2 init-sub-state data)
                       (multiply-depth-distance))) ;; 1685186100

