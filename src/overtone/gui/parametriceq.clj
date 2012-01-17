(ns overtone.gui.parametriceq
  (:use [seesaw core graphics make-widget color]
        [overtone.gui.dial])
  (:require [seesaw.bind :as bind])
  (:import [java.awt Color Paint Stroke BasicStroke GradientPaint
            LinearGradientPaint RadialGradientPaint MultipleGradientPaint]
          [java.awt.geom Point2D$Float Point2D$Double CubicCurve2D$Double]))

(def ^{:private true} curve-style 
  (style    :stroke 4.0 
            :foreground (color :white)))

(def ^{:private true} inner-line-style 
  (style    :stroke 1.5 
            :foreground (color 0 200 0 200)))

(def ^{:private true} line-style 
  (style    :stroke 5.0 
            :foreground (color 200 200 200 150)))

(def ^{:private true} control-node-style 
  (style    :stroke 1.5
            :foreground (color 255 255 255 150)
            :background (java.awt.RadialGradientPaint.
                (float (:x c1)) (float (:y c1)) (float (:radius c1))
                (float-array [0.1 0.5 0.8])
                (into-array java.awt.Color [java.awt.Color/BLACK
                java.awt.Color/WHITE java.awt.Color/ORANGE]))))



(defn distance 
  "Euclidean distance between 2 points"
  [[x1 y1] [x2 y2]]                     
  (Math/sqrt
   (+ (Math/pow (- x1 x2) 2)
      (Math/pow (- y1 y2) 2))))

(defn- create-init-state
  []
  { :left 
    { :x      0.0    
      :y      250.0
      :radius 10.0}
    :right
    { :x      590.0    
      :y      250.0
      :radius 10.0}
    :control-one
    { :x      200.0    
      :y      250.0
      :radius 10.0}
    :control-two
    { :x      400.0    
      :y      250.0
      :radius 10.0}
    :current-node nil})

(defn paint-curve [nodes g]
  (let []
    ))

(defn- paint-node 
  [state g]
  )


(defn paint-parametric-eq
  [state c g]
  (let [w               (width c)
        h               (height c)
        l               (:left state)
        r               (:right state)
        c1              (:control-one state)
        c2              (:control-two state)]
    


    (draw g
      (circle (:x l) (:y l) (:radius l))      control-node-style)
    
    (draw g
      (circle (:x r) (:y r) (:radius r))      control-node-style)

    (draw g
      (circle (:x c1) (:y c1) (:radius c1))   control-node-style)
    
    (draw g
      (circle (:x c2) (:y c2) (:radius c2))   control-node-style)
    
    (draw g
      (line (:x l) (:y l) (:x c1) (:y c1))    control-node-style  
      (line (:x c1) (:y c1) (:x c2) (:y c2))  control-node-style
      (line (:x c2) (:y c2) (:x r) (:y r))    control-node-style

      (doto 
        (new java.awt.geom.CubicCurve2D$Double) 
          (.setCurve  (:x l) (:y l) (:x c1) (:y c1) (:x c2) (:y c2) (:x r) (:y r))) 
            curve-style
      (doto 
        (new java.awt.geom.CubicCurve2D$Double) 
          (.setCurve  (:x l) (:y l) (:x c1) (:y c1) (:x c2) (:y c2) (:x r) (:y r)))
            inner-line-style)))



(defn- node-collision-check
  [state e]
  (let [x     (.getX e)
        y     (.getY e)
        rs    (atom state)]
    (doseq [[id params] state]
      (do
        (if 
          (and 
            (not (nil? params)) 
            (>= (:radius params) (distance [x y] [(:x params) (:y params)])))
        (do 
          (swap! rs #(assoc % :current-node id))))))
      @rs))

(defn- update-current-control-point
  [state e]
  (let [rs    (atom state)
        x     (.getX e)
        y     (.getY e)]
    (if (not (nil? (:current-node state)))
      (do
      (swap! rs (fn [s] (assoc-in (assoc-in s [(:current-node s) :x] x) [(:current-node s) :y] y)))))
    @rs))


(defn- on-pressed
  [state e]
  (node-collision-check state e))

(defn- on-dragged
  [state e]
    (update-current-control-point state e))
    

(defn- on-released
  [state e]
    (assoc state :current-node nil))

(def state (atom nil))

(defn parametric-eq-widget
  [state-atom]
  (let [c (canvas :id :parametric-eq
                  :background (color :black)
                  :paint #(paint-parametric-eq @state-atom %1 %2))
        panel (border-panel :center c)]
        
    (listen c
      :mouse-pressed  #(swap! state-atom on-pressed %)
      :mouse-dragged  #(swap! state-atom on-dragged %) 
      :mouse-released #(swap! state-atom on-released %))

    panel))

(defn- update-node-state
  [state params]
)

(defn parametric-eq
  []
  (invoke-now
    (let [state-atom  (atom (create-init-state))
          eq          (parametric-eq-widget state-atom)
          f           (frame :title    "ParametricEQ"
                             :minimum-size [601 :by 502]
                             :content  (border-panel
                                         :border 5 :hgap 5 :vgap 5
                                         :center eq)
                             :on-close :dispose)]
      (bind/bind state-atom (bind/b-do [v] (repaint! eq)))

      (with-meta {:frame (-> f pack! show!)
                  :state state-atom }
                 {:type :sequencer}))))

(comment 
  (use 'overtone.gui.parametriceq)
  (parametric-eq)
)


