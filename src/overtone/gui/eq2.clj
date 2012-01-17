(ns overtone.gui.eq2
  (:use [seesaw core graphics make-widget color]
        [overtone.gui.dial])
  (:require [seesaw.bind :as bind])
  (:import [java.awt Color Paint Stroke BasicStroke GradientPaint
            LinearGradientPaint RadialGradientPaint MultipleGradientPaint]
          [java.awt.geom Point2D$Float Point2D$Double CubicCurve2D$Double GeneralPath]))

(def ^{:private true} curve-style 
  (style    :stroke 4.0 
            :foreground (color :white)))

(def ^{:private true} inner-line-style 
  (style    :stroke 1.5 
            :foreground (color 0 200 0 200)
            :background (color 255 0 0)))

(def ^{:private true} line-style 
  (style    :stroke 5.0 
            :foreground (color 200 200 200 150)
            :background (java.awt.GradientPaint.
                          (float 300) (float 500) (color 255 255 0 100)
                          (float 300) (float 0) (color 0 0 0 0))))

(def ^{:private true}  control-line-style
  (style    :stroke 1.5
            :foreground (color 255 255 255 150)))

(defn-  control-node-style
  [cx cy radius] 
  (style    :stroke 1.5
            :foreground (color 255 255 255 150)
            :background (java.awt.RadialGradientPaint.
                (float cx) (float cy) (float radius)
                (float-array [0.6 0.99])
                (into-array java.awt.Color [(color 0 255 0 75) (color 0 255 0 175)]))))



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
    :current-node nil

    ; :control-node       {:x 300 :y 250 :radius 10}
    ; :control-dimension  {:width 200.0 :height 500.0}
    ; :bounds-thickness   5.0



    })


(defn cubic-curve 
  [x1 y1 cx1 cy1 cx2 cy2 x2 y2]
  (doto 
    (new java.awt.geom.CubicCurve2D$Double) 
      (.setCurve  
        (float x1) (float y1) 
        (float cx1) (float cy1) 
        (float cx2) (float cy2) 
        (float x2) (float y2))))    

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
      (doto
        (new java.awt.geom.GeneralPath)
          (.moveTo (float 0)  (float h))
          (.lineTo (:x l) (:y l))
          (.append (cubic-curve  (:x l) (:y l) (:x c1) (:y c1) (:x c2) (:y c2) (:x r) (:y r)) true)
          (.lineTo (float w)  (float h))
          (.lineTo (float 0) (float h))) line-style

      (line (:x l) (:y l) (:x c1) (:y c1))    control-line-style  
      (line (:x c1) (:y c1) (:x c2) (:y c2))  control-line-style
      (line (:x c2) (:y c2) (:x r) (:y r))    control-line-style

      (circle (:x l) (:y l) (:radius l))      (control-node-style (:x l) (:y l) (:radius l))
      (circle (:x r) (:y r) (:radius r))      (control-node-style (:x r) (:y r) (:radius r))
      (circle (:x c1) (:y c1) (:radius c1))   (control-node-style (:x c1) (:y c1) (:radius c1))
      (circle (:x c2) (:y c2) (:radius c2))   (control-node-style (:x c2) (:y c2) (:radius c2)))))
            



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


