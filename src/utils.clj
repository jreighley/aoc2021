(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-file [f]
    (->> (slurp f)
         (s/split-lines)))

(defn read-number-file [f]
  (->> f
       (read-file)
       (map #(Long/parseLong %))))

(comment (read-number-file  "resources/day1"))
(comment (read-file "resources/day2"))