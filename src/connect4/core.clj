(ns connect4.core
  (:gen-class)
  (:require [clojure.term.colors :as colors]))

;; definition of board and players
(def init-board (vec (repeat 42 nil)))

(def four-in-a-row (atom []))

(def printed-piece {:x "X" 
                    :o "O" 
                    nil " "})

(def color-piece {:x #'colors/red
                  :o #'colors/cyan
                  nil #'colors/white})

(def next-player {:x :o 
                  :o :x})


;; game constants
(def FIRST-PLAYER :x)

(def X-WINS 1000)
(def O-WINS -1000)
(def TIE 0)

(def DEFAULT-MAX-DEPTH 3)
(def INF 1000000)
(def NINF -1000000)


;; command line options
(def max-depth (atom DEFAULT-MAX-DEPTH))
(def show-score (atom false))


;; display board and winner
(defn print-row [row]
  (print "│ ")
  (print (colors/bold ((color-piece (first row)) (printed-piece (first row)))))
  (doseq [field (rest row)] 
    (print " │ ")
    (print (colors/bold ((color-piece field) (printed-piece field)))))
  (print " │"))

(defn print-board [board]
  (let [rows (partition 7 board)]
    (println)
    (print-row (first rows))
    (println)
    (doseq [row (rest rows)] 
      (println (clojure.string/join (repeat 29 "—")))
      (print-row row)
      (println))
    (println (clojure.string/join (repeat 29 "—")))
    (doseq [col (range 1 8)] (print (str "  " col " ")))
    (println)
    (println)))

(defn print-winner [winner]
  (case winner
    :x (println ((color-piece :x) "Player X wins!"))
    :o (println ((color-piece :o) "Player O wins!"))
    (println ((color-piece nil) "It's a tie!"))))


;; update and query board
(defn drop-piece [board column player]
  (let [square (last (keep-indexed #(when (and (nil? %2) (= column (mod %1 7))) %1) board))]
    (assoc board square player)))

(defn free-columns 
  "which columns aren't full?"
  [board]
  (let [top-row (first (partition 7 board))]
    (keep-indexed #(when (nil? %2) %1) top-row)))

(defn squares-with-piece [board player]
  (set (keep-indexed #(when (= player %2) %1) board)))

(defn four-in-a-row? [board player]
  (let [set-of-squares (squares-with-piece board player)]
    (some #(every? set-of-squares %) @four-in-a-row)))

(defn game-over? [board]
  (cond 
    (four-in-a-row? board :x) :x
    (four-in-a-row? board :o) :o
    (not-any? nil? board) :tie
    :else false))


;; minimax with alpha-beta pruning
(declare -best-move)

(defn evaluate-position [board player]
  (let [full-board (squares-with-piece (replace {nil player} board) player)
        all-fours (filter #(every? full-board %) @four-in-a-row)
        player-score (apply + (map #(inc (quot (first %) 7)) all-fours))]
    player-score))

(defn positional-score [board]
    (- (evaluate-position board :x) (evaluate-position board :o)))

(defn score [board column player depth alpha beta]
  (let [new-board (drop-piece board column player)
        this-score (case (game-over? new-board)
                     :x X-WINS
                     :o O-WINS
                     :tie TIE
                     (if (= depth @max-depth)
                       (positional-score new-board)
                       (score new-board
                              (-best-move new-board (next-player player) (inc depth) alpha beta)
                              (next-player player)
                              (inc depth)
                              alpha
                              beta)))]
    (when (and @show-score (= depth 0))
      (println (printed-piece player) (inc column) (float this-score)))
    this-score))

(defn minimax [board valid-moves player depth alpha beta scores]
  (if (seq valid-moves)
    (let [move (first valid-moves)
          s (score board move player depth alpha beta)
          new-alpha (if (= player :x) (max s alpha) alpha)
          new-beta (if (= player :o) (min s beta) beta)
          remaining-moves (if (> new-alpha new-beta) nil (rest valid-moves))]
      (recur board remaining-moves player depth new-alpha new-beta (conj scores [move s])))
    scores))

(defn -best-move [board player depth alpha beta]
  (let [valid-moves (free-columns board)
        ; scores (map #(vector % (score board % player depth alpha beta)) valid-moves)
        scores (minimax board valid-moves player depth alpha beta [])
        sorted-scores (sort-by #(second %) (if (= player :x) > <) (shuffle scores))
        ; sorted-scores (sort-by #(second %) (if (= player :x) > <) scores)
        ]
    (first (first sorted-scores))))

(defn best-move [board player]
  (-best-move board player 0 NINF INF))


;; input and check human move
(defn human-move [board]
  (let [valid-moves (map inc (free-columns board))]
    (loop []
      (print (str "Enter column " (seq valid-moves) ": "))
      (flush)
      (let [input (read-line)]
        (if (and (= 1 (count input)) (some (set (apply str valid-moves)) input))
          (dec (read-string input))
          (do
            (println "Invalid move.")
            (recur)))))))


;; main game functions
(defn init-four-in-a-row!
  "initialize all four-in-a-row combinations
  top-left: 0, top-right: 6, bottom-left: 35, bottom-right: 41"
  []
  (let [horizontal (partition 4 (for [y (range 6) x (range 4) d (range 4)] (+ (* y 7) x d)))
        vertical (partition 4 (for [d (range 3) x (range 7) y (range 4)] (+ x (* y 7) (* d 7))))
        diagonal-pos (partition 4 (for [d (range 3) x (range 4) y (range 4)] (+ x (* y 8) (* d 7))))
        diagonal-neg (partition 4 (for [d (range 3) x (range 3 7) y (range 4)] (+ x (* y 6) (* d 7))))]
    (swap! four-in-a-row into (concat horizontal vertical diagonal-pos diagonal-neg))))

(defn game-loop [board player human-playing? human-turn?]
  (print-board board)
  (let [game-over (game-over? board)]
    (if-not game-over
      (if human-playing?
        (if human-turn?
          (recur (drop-piece board (human-move board) player) (next-player player) true false)
          (recur (drop-piece board (best-move board player) player) (next-player player) true true))
        (recur (drop-piece board (best-move board player) player) (next-player player) false false))
      (do 
        (reset! four-in-a-row [])
        (print-winner game-over)))))

(defn -main 
  ([]
   (-main "-"))
  ([piece] 
   (init-four-in-a-row!)
   (case (clojure.string/upper-case piece)
     "X" (game-loop init-board FIRST-PLAYER true true)
     "O" (game-loop init-board FIRST-PLAYER true false)
     "-" (game-loop init-board FIRST-PLAYER false false)
     (do (println "Usage: connect4 [ - X O] [0 1 2 3 4 5 6 7 8 9]")
         (println "Player must be – (computer vs. computer), X (human first), O (computer first)")
         (println "Default: computer vs. computer at maximum depth" DEFAULT-MAX-DEPTH))))
  ([piece depth]
   (if (and (= 1 (count depth)) (some (set "0123456789") depth))
     (do
       (reset! max-depth (read-string depth))
       (-main piece))
     (do (println "Usage: connect4 [ - X O] [0 1 2 3 4 5 6 7 8 9]")
         (println "Maximum depth must be between 0 and 9")
         (println "Default: computer vs. computer at maximum depth" DEFAULT-MAX-DEPTH))))
  ([piece depth _]
   (reset! show-score true)
   (-main piece depth)))
