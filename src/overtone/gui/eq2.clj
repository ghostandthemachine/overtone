(ns overtone.gui.eq2
  (:use [seesaw core graphics make-widget color]
        [overtone.gui.dial]
        [overtone.gui.toolbelt])
  (:require [seesaw.bind :as bind])
  (:import [java.awt Color Paint Stroke BasicStroke GradientPaint
            LinearGradientPaint RadialGradientPaint]
           [java.awt.geom Point2D$Float Point2D$Double CubicCurve2D$Double QuadCurve2D GeneralPath]
           [java.lang.Math]))

(defn cos 
  [deg]
  (Math/cos (Math/toDegrees deg)))

(defn cos 
  [deg]
  (Math/sin (Math/toDegrees deg)))

(defn linear-gradient
  [x1 y1 color1 x2 y2 color2]
  (java.awt.GradientPaint.
    (float x1) (float y1) color1
    (float x2) (float y2) color2))

(defn radial-gradient
  [cx cy radius stops colors]
  (java.awt.RadialGradientPaint.
    (float cx) (float cy) (float radius)
    (float-array stops)
    (into-array java.awt.Color colors)))

(defn cubic-curve 
  [x1 y1 cx1 cy1 cx2 cy2 x2 y2]
  (doto 
    (new java.awt.geom.CubicCurve2D$Double) 
      (.setCurve  
        (float x1) (float y1) 
        (float cx1) (float cy1) 
        (float cx2) (float cy2) 
        (float x2) (float y2))))

(defn quad-curve
  [x1 y1 ctrl-x ctrl-y x2 y2]
  (doto 
      (new java.awt.geom.QuadCurve2D$Double) 
        (.setCurve  
          (float x1) (float y1) 
          (float ctrl-x) (float ctrl-y) 
          (float x2) (float y2))))


(def ^{:private true} curve-style 
  (style    :stroke 4.0 
            :foreground (color :black)))
(def ^{:private true} inner-line-style 
  (style    :stroke 1.5 
            :foreground (color 0 200 0 200)
            :background (color 255 0 0)))
(def ^{:private true} line-style 
  (style    :stroke 5.0 
            :foreground (color 200 200 200 150)
            :background (radial-gradient 0.5 0.5 0.5 [0.2 0.7] [(color :red) (color :green)])))
(def ^{:private true}  control-line-style
  (style    :stroke 1.5
            :foreground (color 255 255 255 150)))
(def ^{:private true}  param-bounds-style
  (style    :stroke 1.5
            :foreground (color 0 255 0 200)
            :background (color 0 255 0 50)))
(def ^{:private true}  grid-line-style
  (style    :stroke 1.0
            :foreground (color 200 200 200)))
(def ^{:private true}  grid-line-divider-style
  (style    :stroke 2.0
            :foreground (color 200 200 200)))
(defn-  control-node-style
  [cx cy radius] 
  (style    :stroke 1.5
            :foreground (color 255 255 255 150)
            :background (color :yellow)))


(defn- create-init-state
  [freq gain q]
  { :freq     freq
    :gain     gain
    :q        q
    :bypass?  false})



(defn- paint-base
  [g w h]
  (let [horizontal-step-size      (/ (* w 1.0) 11)
          horizontal-sub-step-size  (/ horizontal-step-size 5)
          vertical-step-size        (/ h 6)]
      (dotimes [i 11]
        (let [x1  (* horizontal-step-size i)
              y1  0
              x2  x1
              y2 h]
          (draw g (line x1 y1 x2 y2) grid-line-divider-style)
        (dotimes [j 4]
          (let [sub-x (+ x1 (+ horizontal-sub-step-size (* horizontal-sub-step-size j)))]
            (draw g (line sub-x y1 sub-x y2) grid-line-style)))))

      (dotimes [i 5]
        (let [x1  0
              y1  (+ vertical-step-size (* vertical-step-size i))
              x2  w
              y2 y1]
          (draw g (line x1 y1 x2 y2) grid-line-style)))
)) 

  
(defn- paint-controls
  [state]
  (let [freq    (map-value 0 1 0 1 (:freq state))
        gain    (map-value -20 20 1 0 (:gain state))
        q       (map-value 1 20 0.0 0.10 (:q state))]
  ))  

(defn- calculate-freq-transform
  [state w h]
  (let [q         (if 
                      (> (:q state) 10.0)
                      [10 (mod (:q state) 10)]
                      [(:q state) 0.0])
        q-width1  (- (map-value 1 20 0.0 0.6 (first q)) 0.3) 
        q-width2  (* -1 q-width1)
        freq-x    (float (map-value 12 22000 0.0 1.0 (:freq state)))
        gain-y    (float (map-value 0 40 0 h (* -1(- (:gain state) 20))))
        ctrl-x1   (float (* w (+  freq-x q-width1)))
        ctrl-y1   (float gain-y)
        ctrl-x2   (float (* w (+ freq-x q-width2)))
        ctrl-y2   ctrl-y1]

    { :x          ctrl-x1
      :y          0
      :width      (- ctrl-x2 ctrl-x1)
      :height     h
      :freq-x     freq-x
      :gain-y     gain-y
      :ctrl-x1    (float (* w (+  freq-x q-width1)))
      :ctrl-y1    (float gain-y)
      :ctrl-x2    (float (* w (+ freq-x q-width2)))
      :ctrl-y2    ctrl-y1}
  ))

(defn- calculate-q-values
  [state]
  (let [q   (:q state)
        q-mapped (map-value 1 20 0 90 q)]
    
  ))

(defn paint-parametric-eq
  [state c g]
  (let [w       (width c)
        h       (height c)] 

    (paint-base g w h) 
    
    (let [gain      (* -1 (- (:gain state) 20))
          gain-y    (float (map-value -20 20 0 h gain))

          q         (if 
                      (> (:q state) 10.0)
                      [-10 (mod (:q state) -10)]
                      [(:q state) 0.0])
          qy        (* h (* 0.1 (last q)))
                       
          q-width1  (* -1 (+ 0.005 (* 0.25 (/ (first q) 20.0))))
          q-width2  (+ 0.005 (* 0.25 (/ (first q) 20.0)))

          freq-x    (float (map-value 12 22000 0 w (:freq state)))
          
          x1        (float 0)
          y1        (float (/ h 2))
          ctrl-x1   (float (+ freq-x q-width1))
          ctrl-y1   (float gain-y)
          ctrl-x2   (float (+ freq-x q-width2))
          ctrl-y2   ctrl-y1
          x2        (float w)
          y2        y1

          rect-transform (calculate-freq-transform state w h)]

    ; (draw g
    ;   (doto
    ;     (new java.awt.geom.GeneralPath)
    ;       (.moveTo (float 0)  (float h))
    ;       (.lineTo x1 y1)
    ;       (.append (quad-curve 
    ;                   0
    ;                   (/ h 2)
    ;                   (:x rect-transform)
    ;                   (+ gain-y qy)
    ;                   freq-x
    ;                   gain-y) true)
    ;       (.append (quad-curve 
    ;                   freq-x
    ;                   gain-y
    ;                   (+ (:x rect-transform) (:width rect-transform))
    ;                   (+ gain-y qy)
    ;                   w
    ;                   (/ h 2)) true)
    ;       (.lineTo (float w)  (float h))
    ;       (.lineTo (float 0) (float h))) curve-style)

    ; (draw g 
    ;   (circle 0 (/ h 2) 10) (control-node-style x1 y1 20)
    ;   (circle w (/ h 2) 10) (control-node-style x1 y1 20)
    ;   (circle (:ctrl-x1 rect-transform) (:ctrl-y1 rect-transform) 10) (control-node-style x1 y1 20)
    ;   (circle (:ctrl-x2 rect-transform) (:ctrl-y2 rect-transform) 10) (control-node-style x1 y1 20)

    ;   )


    (draw g 
      (rect (:x rect-transform) (:y rect-transform) (:width rect-transform) (:height rect-transform)) param-bounds-style
      )
      )))
            
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
     (if (:current-node state)
       (update-in state [(:current-node state)] assoc :x (.getX e) :y (.getY e))
       state))

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
                  :background (color :white)
                  :paint #(paint-parametric-eq @state-atom %1 %2))
        panel (border-panel :minimum-size [400 :by 200]
                            :center c)]
        
    ; (listen c
    ;   :mouse-pressed  #(swap! state-atom on-pressed %)
    ;   :mouse-dragged  #(swap! state-atom on-dragged %) 
    ;   :mouse-released #(swap! state-atom on-released %))

    panel))

(defn- update-node-state
  [state params]
)

(defn- control-slider-group
  [state-atom id vmin vmax l]
  (let [state @state-atom
        label (label :text l :h-text-position :center)
        slider (dial :value (id state) :min vmin :max vmax)
                    ; :orientation :vertical)
        vsp (vertical-panel :items [label slider label])]
    (bind/bind slider (bind/b-do [v] (swap! state-atom assoc id (float v))))
               vsp))

(defn parametric-eq
  []
  (invoke-now
    (let [state-atom    (atom (create-init-state 11000 20 1))
          eq            (parametric-eq-widget state-atom)
          gain-control  (control-slider-group state-atom :gain 0 40 "gain")
          q-control     (control-slider-group state-atom :q 1 20 "q")
          freq-control  (control-slider-group state-atom :freq 12 22000 "freq")
          control-panel (horizontal-panel :items [gain-control q-control freq-control])
          f             (frame :title    "ParametricEQ"
                               :content  (border-panel
                                         :border 5 :hgap 5 :vgap 5
                                         :center eq
                                         :south control-panel)
                               :on-close :dispose)]
      (bind/bind state-atom (bind/b-do [v] (repaint! eq)))

      (with-meta {:frame (-> f pack! show!)
                  :state state-atom }
                 {:type :sequencer}))))

(comment 
  (use 'overtone.gui.parametriceq)
  (parametric-eq)
)


