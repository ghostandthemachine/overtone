(ns overtone.gui.npianoroll
  (:use [overtone.sc server]
        [overtone.music time]
        [overtone.gui.control :only [synth-controller]]
        [seesaw core]
        [seesaw.color :only [color]]
        [seesaw.graphics :only [style draw rounded-rect rect line]]
        [seesaw.swingx :only [hyperlink]])
  (:require [seesaw.bind :as bind]))

(def ^{:private true} note-types [:white :black :white :black :white :white :black :white :black :white :black :white])
(def ^{:private true} MEASURE_WIDTH 150)
(def ^{:private true} NOTE_HEIGHT 10)
(def ^{:private true} NUM_MEASURES 4)
(def ^{:private true} NUM_OCTAVES 4)
(def ^{:private true} STEPS_PER_BEAT 4)

(def ^{:private true} ACTIVE_NOTE_PADDING 2)

(def ^{:private true} NUM_BEATS 4)
(def ^{:private true} NUM_BARS 4)

(defn- make-initial-state [metro steps notes init-vals]
  {:playing?        false
   :metronome       metro
   :steps           steps
   :step            0
   :notes           (vec (repeat notes (vec (repeat steps {:duration 0 :velocity 0.0}))))

   :measure-width     MEASURE_WIDTH
   :bar-width         (/ MEASURE_WIDTH 4)
   :beat-width        (/ MEASURE_WIDTH 4)
   :note-width        (/ (/ MEASURE_WIDTH 4) 4)
   :note-height       NOTE_HEIGHT
   :piano-roll-width  (* notes (/ (/ MEASURE_WIDTH 4) 4))

   :cell-x-start      0      ;; hold the starting cell location of the current note
   :cell-y-start      0
   :start-x           0      ;; hold the starting cell of the current gesture
   :start-y           0
   
   :active-note       []})


(defn- evaluate-note-params [state e]
  (let [start-x     (int (/ (.getX e) (state :note-width)))
        start-y     (int (/ (.getY e) (state :note-height)))
        i           (atom 0)
        note-row    ((state :notes) start-y)
        state-atom (atom state)]

    (println "eval note start"start-x start-y @i)
    (println "eval note start" (@state-atom :cell-x-start) (@state-atom :cell-y-start) (@state-atom :start-x) (@state-atom :start-y))

    ; assume this is a new note first instead of reseting these every time the conditions fail
    (swap! state-atom 
      (fn [s] 
        (assoc 
          (assoc 
            (assoc
              (assoc s 
                :start-x (int start-x))
                :start-y (int start-x))
                :cell-x-start (int start-x))
                :cell-y-start (int start-y))))
    
    (println "eval note end" (@state-atom :cell-x-start) (@state-atom :cell-y-start) (@state-atom :start-x) (@state-atom :start-y))

    (for [note note-row]
      (do 
        (if 
          (and (<= (- start-x 1) (+ i (note :duration)))
               (>= (- start-x 1) i))
            (do     ; in the bounds of an old note
              (swap! state-atom (fn [s] (update-in s [:start-x] (fn [_] (int start-x)))))))
          (swap! i inc)))
    @state-atom))


(defn- toggle-playing [state]
  (update-in state [:playing?] not))

(defn- set-entry [state v]
  (assoc-in (:notes state) [(state :cell-y-start) (state :cell-x-start)] v))

(defn- get-entry [state  note step]
  (get-in (:notes state) [note step]))

(defn- clear-row [state note]
  (assoc-in state [:notes note :value] (vec (repeat (:steps state) false))))

(defn- step-player
  [state-atom beat]
  (let [state @state-atom]
    (when (:playing? state)
      (let [metro (:metronome state)
            steps (:steps state)
            index (mod beat steps)
            next-beat (inc beat) ]
        (swap! state-atom assoc-in [:step] index)
        (doseq [{:keys [inst value]} (:notes state)]
          (when (nth value index)
            (at (metro beat) (inst))))
        (apply-at (metro next-beat) #'step-player
                  [state-atom next-beat])))))


(def ^{:private true} measure-border-style
  (style 
    :stroke 2.0
    :foreground (color 100 100 100 200)))
(def ^{:private true} note-border-style              
  (style 
    :stroke 1.0
    :foreground (color 100 100 100 100)))
(def ^{:private true} beat-note-border-style         
  (style 
    :stroke 1.25
    :foreground (color 100 100 100 175)))
(def ^{:private true} white-note-track-border-style  
  (style 
    :stroke 0.25
    :foreground (color 100 100 100 100)
    :background (color 255 255 255)))
(def ^{:private true} black-note-track-border-style  
  (style 
    :stroke 0.25
    :foreground (color 100 100 100 100)
    :background (color 180 180 180)))
(def ^{:private true} active-notes-style
  (style 
    :stroke 1.0
    :background (color 0 255 0 150)
    :foreground (color 0 150 0)))


(defn- paint-active-notes
  [state g]
  
      ; (let [x   (double (+ ACTIVE_NOTE_PADDING (* k note-width)))
      ;       y   (double (+ ACTIVE_NOTE_PADDING (* n note-height)))
      ;       w   (- (* (note-data :duration) note-width) (* 2 ACTIVE_NOTE_PADDING))
      ;       h   (- note-height (* 2 ACTIVE_NOTE_PADDING))]
      ;   (draw g
      ;         (rounded-rect x y w h 3 3)
      ;         active-notes-style))
              )

(defn- paint-piano-roll
  [state c g]
  (let [w               (width c)
        h               (height c)
        num-octaves     NUM_OCTAVES
        num-measures    NUM_MEASURES
        measure-width   MEASURE_WIDTH
        bar-width       (/ measure-width 4)
        beat-width      (/ measure-width 4)
        note-width      (/ beat-width 4)
        note-height     NOTE_HEIGHT
        num-notes       (* NUM_OCTAVES 12)
        total-height    (* num-notes note-height)
        resolution      (* NUM_BEATS 4)    ;; 4 will ultimatley be set by time sig param
        num-beats       NUM_BEATS]


    ;; draw the vertical notes using the note map as a key for black or white
    (dotimes [i num-measures]
      (dotimes [note-y num-notes]
        (let [ntx (* i measure-width)
              nty (* note-y note-height)
              note-type (nth note-types (mod note-y 12))]
          (cond
            (= note-type :black)
            (do
            (draw g
                  (rect ntx nty measure-width note-height)
                  black-note-track-border-style)
                  )
            (= note-type :white)
            (draw g
                  (rect ntx nty measure-width note-height)
                  white-note-track-border-style))))

      ;; draw the beat and note divisor lines            
      (dotimes [note-x resolution]
        (let [nx (+ (* i measure-width) (* note-x note-width))]
          (if
            (= 0 (mod note-x num-beats))
            (draw g
                  (line nx 0 nx total-height) beat-note-border-style)
            (draw g
                  (line nx 0 nx total-height) note-border-style))))

      ;; measure border
      (draw g
            (rect (* i measure-width) 0 measure-width (- total-height 1))
            measure-border-style))

    ;; paint the active notes last
    (paint-active-notes state g)
    ))

(defn- on-pressed [state e]
  (let [v 1.0]
    (set-entry (evaluate-note-params state e) v)))

(defn- on-piano-roll-dragged [state e])

(def s (atom nil))


(defn- note-grid [state-atom]
  (let [state @state-atom
        c (canvas :background :white
                  :paint #(paint-piano-roll @state-atom %1 %2)
                  :preferred-size [601 :by 502])]
    (listen c 
      :mouse-pressed #(swap! state-atom on-pressed %)
      :mouse-dragged #(swap! state-atom on-piano-roll-dragged %))
    c))

(defn piano-roll
  [metro octaves steps & [init-vals]]
  (invoke-now
    (let [num-notes    (* octaves 12)
          state-atom   (atom (make-initial-state metro steps num-notes init-vals))
          play-btn     (button :text "play")
          bpm-spinner  (spinner :model (spinner-model (metro :bpm) :from 1 :to 10000 :by 1)
                                :maximum-size [60 :by 100])
          controls-btn (button :text "controls")
          control-pane (toolbar :floatable? false
                                :items [play-btn
                                        :separator
                                        bpm-spinner
                                        [:fill-h 5]
                                        "bpm"
                                        :separator
                                        controls-btn])
          note-grid    (note-grid state-atom)

          f (frame :title    "Piano Roll"
                   :content  (border-panel
                               :border 5 :hgap 5 :vgap 5
                               :north control-pane
                               ; :west (grid-panel :columns 1
                               ;                   :items inst-btns)
                               :center note-grid)
                   :on-close :dispose)]
      (bind/bind bpm-spinner (bind/b-do [v] (metro :bpm v)))
      (bind/bind state-atom (bind/b-do [v] (repaint! note-grid)))

      (swap! s (fn [_] @state-atom))    

      (listen play-btn :action
              (fn [e]
                (let [playing? (:playing? (swap! state-atom toggle-playing))]
                  (config! play-btn :text (if playing? "stop" "play"))
                  (if playing?
                    (step-player state-atom (metro))))))

      (with-meta {:frame (-> f pack! show!)}
                 {:type :sequencer}))))




(comment
  (use 'overtone.live)
  (use 'overtone.gui.pianoroll)
  (def m (metronome 128))
  (piano-roll m 4 64)
)