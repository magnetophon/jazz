(ns jazz.core
  (require [quil.core :as q]))
(use 'overtone.core)
(connect-external-server)
(use 'overtone.inst.sampled-piano)
(import java.awt.Toolkit)

(def piano sampled-piano)
(piano)

;; this is one possible implementation of play-chord,
;; it was not shown in the video
(defn play-chord [a-chord]
  (doseq [note a-chord] (piano note)))

(play-chord (chord :C4 :major))

(defonce metro (metronome 120))
(metro)

(defn screenX
  "get the screen width"
  []
  (let [size (.getScreenSize (Toolkit/getDefaultToolkit))] (.getWidth size)))

(defn screenY
  "get the screen height"
  []
  (let [size (.getScreenSize (Toolkit/getDefaultToolkit))] (.getHeight size)))


(defn mouseX
  "get mouse X position scaled from 0 to 1"
  []
  (let [point (.. java.awt.MouseInfo getPointerInfo getLocation)] (/ (.getX point) (- (screenX) 1))))

(defn mouseY
  "get mouse Y position scaled from 0 to 1, and flipped"
  []
  (let [point (.. java.awt.MouseInfo getPointerInfo getLocation)] (+ 1 (* -1 (/ (.getY point) (- (screenY) 1))))))

(defn my-strum
  "strum trough"
  [chord-root chord-name inversion nrNotes length]

  (def theChord (chord chord-root chord-name inversion))
  (def mouseNrNotes (int (max 1 (* 4 (mouseX) nrNotes))))
  (def transposeNr (int (* 8 (mouseY))))

  (println chord-root chord-name)
  (println mouseNrNotes "notes in" length "beats")
  (dotimes [i   mouseNrNotes]
    (at (metro (+ (/ (* length i)  mouseNrNotes) (metro)))
        (piano (first (invert-chord theChord (+  transposeNr (mod i 24))))))))

;; (play-song)
;; (defn play-song
;;   "play trough the whole song"
;;   []
;;   (dotimes my-strum)
;;   )

(defn atBeat
  "apply a function at beatNr beats from now"
  [beatNr function args]
  ;; (apply-at (metro (+ beatNr (metro))) function)
  (apply-at (metro (+ beatNr (metro))) function args))

(atBeat 0 my-strum  [:C2 :major 0 5 5/3])
(chordsA)
(stop)

(defn chordsA []
  (atBeat 0  my-strum [:C2 :major  0 3 3/3])
  (atBeat 4  my-strum [:C2 :minor  0 3 1])
  (atBeat 8  my-strum [:A1 :minor7 0 4 4/3])
  (atBeat 12 my-strum [:F1 :major  0 5 5/3])
  (atBeat 15 chordsB []))

(defn chordsB []
  (atBeat 0   my-strum [:A1 :minor 0 5 5/3])
  (atBeat 4   my-strum [:G1 :minor 0 5 5/3])
  (atBeat 8   my-strum [:F1 :minor 0 5 5/3])
  (atBeat 12  my-strum [:G1 :minor 0 5 5/3])
  (atBeat 15 chordsC []))

(defn chordsC []
  (atBeat 0  my-strum [:C2 :m9     0 10 10/3])
  (atBeat 4  my-strum [:C2 :minor  0 7 7/3])
  (atBeat 8  my-strum [:A1 :dim    0 10 10/3])
  (atBeat 12 my-strum [:F1 :major  0 7 7/3])
  (atBeat 15 chordsD []))

(defn chordsD []
  (atBeat 0  my-strum [:A1 :minor 0 12 4])
  (atBeat 4  my-strum [:G1 :minor 0 10 10/3])
  (atBeat 8  my-strum [:F1 :minor 0 12 4])
  (atBeat 12 my-strum [:G1 :minor 0 7 7/3])
  (atBeat 15 chordsE []))

(defn chordsE []
  (atBeat 0  my-strum [:C2 :m9    0 12 4])
  (atBeat 4  my-strum [:C2 :minor 0 7 7/3])
  (atBeat 8  my-strum [:A1 :dim   1 12 4])
  (atBeat 12 my-strum [:F1 :major 2 7 7/3])
  (atBeat 15 chordsF []))

(defn chordsF []
  (atBeat 0  my-strum [:A1 :minor 1 12 4])
  (atBeat 4  my-strum [:G1 :minor 0 12 4])
  (atBeat 8  my-strum [:F1 :minor 0 12 4])
  (atBeat 12 my-strum [:F1 :dim   1 7 7/3])
  (atBeat 15 chordsA []))

;; (defn chordsEnd []
;;   (my-strum 0  beat-num)) (play-chord (chord :A3 :minor)))
;;   (my-strum 4  beat-num)) (play-chord (chord :G3 :minor)))
;;   (my-strum 8  beat-num)) (play-chord (chord :F3 :minor)))
;;   ;; (apply-at (m (+ 16 beat-num)) chordsA m (+ 16 beat-num) [])
;; )

(defn setup []
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 200))                 ;; Set the background colour to
                                      ;; a nice shade of grey.
(defn draw []
  (q/stroke (q/random 255))             ;; Set the stroke colour to a random grey
  (q/stroke-weight (q/random 10))       ;; Set the stroke thickness randomly
  (q/fill (q/random 255))               ;; Set the fill colour to a random grey

  (let [diam (q/random 100)             ;; Set the diameter to a value between 0 and 100
        x    (q/random (q/width))       ;; Set the x coord randomly within the sketch
        y    (q/random (q/height))]     ;; Set the y coord randomly within the sketch
    (q/ellipse x y diam diam)))         ;; Draw a circle at x y with the correct diameter

(q/defsketch circles                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [323 200])                    ;; You struggle to beat the golden ratio
