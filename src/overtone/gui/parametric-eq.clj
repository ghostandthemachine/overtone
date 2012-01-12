(ns overtone.gui.pianoroll
  (:use [seesaw core graphics make-widget color])
  (:require [seesaw.bind :as bind])
  (:import [javax.swing DefaultBoundedRangeModel]))

(def curve-style (style   :stroke 2.0 
                          :foreground (color 0 150 0)))

(defn paint-curve [nodes g]
  (let []
    ))


(defn paint-parametric-eq
  [num-measures num-bars num-beats num-steps-per-beat num-octaves notes active-notes c g]
  (let [w   (width c)
        h   (height c)]
    (draw g
      (rect 0 0 w h) (style :background (color 0 244 0)))))

(defn paramtric-eq-widget
  [num-nodes]
  (let [nodes (atom {})
        eq (canvas  :id :parametric-eq
                            :paint (partial paint-parametric-eq @nodes))
        panel (border-panel :center eq)]


    (listen piano-roll
      :mouse-pressed
        (fn [e])

      :mouse-dragged
        (fn [e])

      :mouse-released
        (fn [e])
    panel)))

(defn create-eq-panel []
  (let [panel (parametric-eq-widget 1)]
          panel))

(defn parametric-eq
  ([]
   (invoke-now
      (let [eq (create-eq-panel)
           panel (border-panel :id :piano-roll :center piano-roll)
           f (-> (frame :title "Parametric EQ Test"
                        :on-close :dispose
                        :minimum-size [601 :by 502]
                        :content panel)
               pack!
               show!)]
        f))))



; (def m {0 {0 {:duration 8 :velocity 1.0}}, {3 {:duration 8 :velocity 1.0}}, {5 {:duration 8 :velocity 1.0}}})