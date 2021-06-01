(ns draftkings-clj.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [clojure.walk :refer [keywordize-keys]]))

(defn determine-players-value
  "Determine a player's value (very simple).
   To be replaced with something more sophisticated."
  [players]
  (map #(assoc % :ratio (/ (:AvgPointsPerGame %) (:Salary %))) players))

(defn csv-data->maps
  "Convert csv file data into a map."
  [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest csv-data)))

(defn csv-file->map
  "Open and read a csv file."
  [filename]
  (with-open [file (io/reader filename)]
    (doall (-> (csv/read-csv file)
               csv-data->maps))))

(defn format-player
  "Format a single line for a player on a team."
  [player]
  (format "%-8s | %-20s | %-4s | %-15s | %-7d | %-6.2f\n"
          (:Position player)
          (:Name player)
          (:TeamAbbrev player)
          (subs ((keyword "Game Info") player) 0 15)
          (:Salary player)
          (:AvgPointsPerGame player)))

;; TODO: Could be cleaned up a lot
(defn format-team
  "Produce a string that provides a team's info."
  [team]
  (let [team (vec team)
        longest-name-length (apply max (map #(count (:Name %)) team))
        team-salary (reduce + (map :Salary team))
        team-projection (reduce + (map :AvgPointsPerGame team))]
    (str
     (format "Team Proj: %.2f\n" team-projection)
     (format "Team Cost: %d\n" team-salary)
     (format "%-8s | %-20s | %-4s | %-15s | %-7s | %-6s\n" "Position" "Player" "Team" "Matchup" "Salary" "Proj")
     (apply str (map #(format-player (get team %)) (range 9))))))

;; TODO: Could handle bad data better (like UNK)
(defn parse-player-data
  "Convert data to its correct type."
  [players]
  (reduce (fn [coll player]
            (conj coll (-> player
                           (update :Salary #(Integer/parseInt %))
                           (update :AvgPointsPerGame #(Double/parseDouble %)))))
          []
          players))

;; TODO: Come up with a way to make this more versatile for other sports / formats
(defn is-valid-team?
  "Determine if a team is valid or not."
  [players]
  (let [position-count (->> players
                            (map :Position)
                            frequencies
                            keywordize-keys)
        team-salary (reduce + (map :Salary players))]

    (cond
      ;; Salary must be $50,000 or less
      (> team-salary 50000) false

      ;; Must have specific positions filled
      (not= (:QB position-count) 1) false
      (not= (:DST position-count) 1) false
      (< (:RB position-count) 2) false
      (< (:WR position-count) 3) false
      (< (:TE position-count) 1) false

      ;; Not all players can be from the same team
      (< (count (set (map :TeamAbbrev players))) 2) false

      :else true)))

(defn determine-lineups
  "Determine the best lineup based on projections."
  [players]
  ;; Take the top X ratio and top X scoring players
  ;; TODO: Much cleaner way to do this
  ;; TODO: Need a more efficient method to pick players
  (let [top-ratio-players
        (set (concat
              (->> players
                   (filter #(= (:Position %) "QB"))
                   (sort-by :ratio)
                   reverse
                   (take 1))
              (->> players
                   (filter #(= (:Position %) "RB"))
                   (sort-by :ratio)
                   reverse
                   (take 2))
              (->> players
                   (filter #(= (:Position %) "WR"))
                   (sort-by :ratio)
                   reverse
                   (take 3))
              (->> players
                   (filter #(= (:Position %) "TE"))
                   (sort-by :ratio)
                   reverse
                   (take 1))
              (->> players
                   (filter #(= (:Position %) "DST"))
                   (sort-by :ratio)
                   reverse
                   (take 1))))
        top-scoring-players
        (set (concat
              (->> players
                   (filter #(= (:Position %) "QB"))
                   (sort-by :AvgPointsPerGame)
                   reverse
                   (take 1))
              (->> players
                   (filter #(= (:Position %) "RB"))
                   (sort-by :AvgPointsPerGame)
                   reverse
                   (take 2))
              (->> players
                   (filter #(= (:Position %) "WR"))
                   (sort-by :AvgPointsPerGame)
                   reverse
                   (take 3))
              (->> players
                   (filter #(= (:Position %) "TE"))
                   (sort-by :AvgPointsPerGame)
                   reverse
                   (take 1))
              (->> players
                   (filter #(= (:Position %) "DST"))
                   (sort-by :AvgPointsPerGame)
                   reverse
                   (take 1))))]
    ;; Take the set of the top X ratio scoring players build X choose 8
    (->> (combo/combinations (union top-ratio-players top-scoring-players) 9)
         (filter is-valid-team?) ;; Make sure team is valid
         ;; Sort the teams by their projection descending
         ;; TODO: Fix sort
         #_(reverse (sort-by (fn [teams]
                    (map #(reduce + (map :AvgPointsPerGame)) teams)))))))

(defn -main
  "Entry point."
  [& _]
  ;; TODO: Better way to read file than harcode?
  (let [players (csv-file->map "resources/DKSalaries.csv")]

    ;; TODO: Add more configuration so less "magic numbers"
    ;; TODO: Optionally remove low salary players
    ;; TODO: Optionally remove injured players
    ;; TODO: Handle any values that look invalid

    (->> players
         parse-player-data
         determine-players-value
         determine-lineups
         (take 5)
         (map format-team)
         (run! println))))

(comment
  @(def players (->> (csv-file->map "resources/DKSalaries.csv")
                     parse-player-data
                     determine-players-value
                     determine-lineups
                     (take 5)
                     ))
  )
