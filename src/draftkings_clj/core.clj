(ns draftkings-clj.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn determine-players-value
  [players])

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn csv-file->map
  "Open and read a csv file"
  [filename]
  (with-open [file (io/reader filename)]
    (doall (-> (csv/read-csv file)
               csv-data->maps))))

(defn print-team
  "Produce a string that provides the team's info."
  [team]
  (let [longest-name-length (apply max (map #(count (:Name %)) team))]
  (format "+%8d+%14d")
  ))

(defn -main
  "I don't do a whole lot ... yet."
  [& _]
  (let [players (csv-file->map "resources/DKSalaries.csv")]

    ;; TODO: Optionally remove low salary players based on CSV
    ;; TODO: Optionally remove injured players based on CSV
    ;; TODO: Handle any values that look invalid in CSV

    (print-team (take 8 players))

    ))

(comment
  (doall (->> (csv-file->map "resources/DKSalaries.csv")
              (take 8)
              (map #(count (:Name %)))
              (apply max)
              ))

  (format "+ %8s + %-14s + %-4s + %-15s + %-7s + %-6s +" "Position" "Player" "Team" "Matchup" "Salary" "Proj")

  )
