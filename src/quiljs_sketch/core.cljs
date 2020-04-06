(ns quiljs-sketch.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [cljs.pprint :as cljspp]))

(defn make-button [state id x y width height color on-click]
  (assoc state
         :buttons (conj (:buttons state) {id {:x x :y y :w width :h height :c color :on-click on-click}})))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb 12 10 10)
  (-> {:color 0
       :lines '[]
       :current_line '[]
       :buttons {}}
      (make-button :red 0 0 50 50 0 #(assoc % :color 0))
      (make-button :orange 50 0 50 50 1 #(assoc % :color 1))
      (make-button :yellow 100 0 50 50 2 #(assoc % :color 2))
      (make-button :light-green 150 0 50 50 3 #(assoc % :color 3))
      (make-button :green 200 0 50 50 4 #(assoc % :color 4))
      (make-button :green-blue 250 0 50 50 5 #(assoc % :color 5))
      (make-button :teal 300 0 50 50 6 #(assoc % :color 6))
      (make-button :light-blue 350 0 50 50 7 #(assoc % :color 7))
      (make-button :blue 400 0 50 50 8 #(assoc % :color 8))
      (make-button :purple 450 0 50 50 9 #(assoc % :color 9))
      (make-button :pink 500 0 50 50 10 #(assoc % :color 10))
      (make-button :light-red 550 0 50 50 11 #(assoc % :color 11))))

(defn render-button [state button-id]
  (let [button (button-id (:buttons state))
        x (:x button)
        y (:y button)
        w (:w button)
        h (:h button)
        c (:c button)]
    (q/fill c 10 10)
    (q/stroke-weight 1)
    (q/rect x y w h))
  state)

(defn update-state [state] state)

(defn draw-state [state]
  (q/background 0 0 10)
  (q/fill (:color state) 10 10)
  (q/stroke (:color state) 10 10)
  (q/stroke-weight 10)
  (let [original-color (:color state)]
    (doseq [[line color] (:lines state)]
      (cond
        (>= (count line) 2) ((q/fill color 10 10) (q/stroke color 10 10) (dorun (reduce (fn [v1 v2] (apply q/line (concat v1 v2)) v2) line)))
        (= (count line) 1) ((q/fill color 10 10) (q/stroke color 10 10) (apply q/point (first line)))))
    (q/fill original-color 10 10)
    (q/stroke original-color 10 10)
    (when (>=(count (:current_line state)) 2) (dorun (reduce (fn [v1 v2] (apply q/line (concat v1 v2)) v2) (:current_line state)))))
  (-> state
      (render-button :red)
      (render-button :orange)
      (render-button :yellow)
      (render-button :light-green)
      (render-button :green)
      (render-button :green-blue)
      (render-button :teal)
      (render-button :light-blue)
      (render-button :blue)
      (render-button :purple)
      (render-button :pink)
      (render-button :light-red)))

(defn mouse-dragged [state event]
  (assoc state
         :current_line (conj (:current_line state) [(:x event) (:y event)])))

(defn mouse-released [state event]
  (assoc state
         :lines (conj (:lines state) [(:current_line state) (:color state)])
         :current_line '[]))

(defn in-rect-bounds? [rect x y]
  (let [rect-x (:x rect)
        rect-y (:y rect)
        rect-w (:w rect)
        rect-h (:h rect)]
    (and (<= rect-x x (+ rect-x rect-w)) (<= rect-y y (+ rect-y rect-h)))))

(defn any-buttons-clicked? [state x y]
  (let [buttons (:buttons state)]
    (first (filter #(in-rect-bounds? (second %) x y) buttons))))

(defn mouse-clicked [state event]
  (let
    [button-clicked (second (any-buttons-clicked? state (:x event) (:y event)))]
    (cond
      (not-empty button-clicked) ((:on-click button-clicked) state)
      :else state)))

(defn key-pressed [state event]
  (when (= (:key event) :d) (cljspp/pprint state))
  state)

; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch quiljs-sketch
    :host "quiljs-sketch"
    :size [600 800]
    :setup setup
    :update update-state
    :draw draw-state
    :mouse-pressed mouse-dragged
    :mouse-dragged mouse-dragged
    :mouse-released mouse-released
    :mouse-clicked mouse-clicked
    :key-pressed key-pressed
    :middleware [m/fun-mode]))
