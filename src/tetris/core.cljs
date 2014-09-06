(ns tetris.core
  (:require [cljs.core.async :refer [<! >! chan put! close! timeout alts!]]
            [domina.events :as e])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(def WIDTH 400)
(def HEIGHT 400)
(def X-TILES 10)
(def Y-TILES 10)

(def moves
  {:RIGHT [1 0]
   :LEFT [-1 0]
   :DOWN [0 1]})

(def charcodes {37 :L
                38 :U
                39 :R
                40 :D
                32 :SPACE})

(def pieces
  {:I [ [-1  0] [ 0  0] [ 1  0] [ 2  0] ]
   :T [ [ 0 -1] [-1  0] [ 0  0] [ 1  0] ]
   :O [ [ 0 -1] [ 1 -1] [ 0  0] [ 1  0] ]
   :J [ [-1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :L [ [ 1 -1] [-1  0] [ 0  0] [ 1  0] ]
   :S [ [ 0 -1] [ 1 -1] [-1  0] [ 0  0] ]
   :Z [ [-1 -1] [ 0 -1] [ 0  0] [ 1  0] ]})

(defn empty-row [] (into [] (repeat X-TILES 0)))
(defn empty-board
  ([n] (into [] (repeat n (empty-row))))
  ([] (empty-board Y-TILES)))

(def initial-game-state
  {:piece nil
   :position nil
   :board (empty-board)})

(def canvas (.getElementById js/document "canvas"))
(set! (.-width canvas) WIDTH)
(set! (.-height canvas) HEIGHT)
(def context (.getContext canvas "2d"))

(def tile-colors {:occupied "#EEE"
                  :unoccupied "#000"
                  :piece "#00F"})

(defn tile-type [piece-locs x y v]
  (cond
    (= v 1) :occupied
    (piece-locs [x y]) :piece
    (= v 0) :unoccupied))

(defn occupied?
  "Is this position occupied already?"
  [{board :board} x y]
  (or (= 1 (nth (nth board y []) x 0))
      (>= y Y-TILES)
      (>= x X-TILES)
      (< x 0)))

(defn valid?
  [{board :board :as state} [x y]]
  (not (occupied? state x y)))

(defn piece-locations [piece [x-org y-org]]
  (mapv (fn [[x y]] [(+ x x-org) (+ y y-org)]) piece))

(defn valid-state? [{:keys [piece position] :as state}]
  (let [piece-locs (piece-locations piece position)]
    (every? (partial valid? state) piece-locs)))

(defn lock-piece [{:keys [piece position board] :as state}]
  (let [piece-locs (piece-locations piece position)
        new-board (reduce (fn [b [x y]] (assoc-in b [y x] 1)) board piece-locs)]
    (-> state
      (assoc :board new-board)
      (assoc :piece (rand-nth (vals pieces)))
      (assoc :position [(/ X-TILES 2) 0]))))

(defn render-tile [tile-height tile-width piece-locs y x v]
  (let [x-pix (* x tile-width)
        y-pix (* y tile-height)
        tile-type (partial tile-type piece-locs)
        color (tile-colors (tile-type x y v))]
    (.beginPath context)
    (.rect context x-pix y-pix tile-width tile-height)
    (set! (.-fillStyle context) color)
    (.fill context)
    (set! (.-lineWidth context) (/ WIDTH X-TILES 10))
    (set! (.-strokeStyle context) "#AAA")
    (.stroke context)))

(defn render [{:keys [board piece position]}]
  (let [tile-height (/ HEIGHT Y-TILES)
        tile-width (/ WIDTH X-TILES)
        piece-locs (set (piece-locations piece position))
        render-tile (partial render-tile tile-height tile-width piece-locs)
        render-row (fn [idx row]
                     (doall (map-indexed (partial render-tile idx) row)))
        render-board (fn [board]
                       (doall (map-indexed render-row board)))]
    (render-board board)))

(defn rotate-coords [[x y]] [(- y) x])
(defn rotate-piece [piece] (mapv rotate-coords piece))

(defn move-coords [mv pos] (mapv + mv pos))
(defn move-piece [pos dir] (move-coords (moves dir) pos))

(defn state-move-piece [{pos :position :as state} dir]
  (let [new-state (assoc state :position (move-piece pos dir))]
    (if (valid-state? new-state) new-state state)))
(defn move-piece-left [state] (state-move-piece state :LEFT))
(defn move-piece-right [state] (state-move-piece state :RIGHT))
(defn move-piece-down [state] (state-move-piece state :DOWN))

(defn remove-row-if-necessary [{board :board :as state}]
  (let [smallboard (into [] (remove #(every? (partial = 1) %) board))
        need (- X-TILES (count smallboard))
        new-board (into [] (concat (empty-board need) smallboard))]
    (assoc state :board new-board)))

(defn lock-piece-if-necessary [state]
  (if-not (valid-state? (assoc state
                               :position
                               (move-piece (:position state) :DOWN)))
    (remove-row-if-necessary (lock-piece state))
    (move-piece-down state)))

(defn timeout-actions [state] (-> state (lock-piece-if-necessary)))

(defn state-rotate-piece [{piece :piece :as state}]
  (let [new-state (assoc state :piece (rotate-piece piece))]
    (if (valid-state? new-state) new-state state)))

(defn state-do [state evt]
  (condp = evt
    :L (move-piece-left state)
    :R (move-piece-right state)
    :D (move-piece-down state)
    :U (state-rotate-piece state)
    :SPACE state
    :TIMEOUT (timeout-actions state)))

(def states (chan))

(def c (chan))
(go-loop []
  (<! (timeout 2000))
  (>! c :TIMEOUT)
  (recur))

(def game-state (lock-piece initial-game-state))

(e/listen! :keypress (fn [evt]
                       (if-let [k (charcodes (:charCode evt))]
                         (put! c k))))

(go-loop [state game-state]
  (let [k (<! c)
        new-state (state-do state k)]
    (render new-state)
    (recur new-state)))

