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

  (reset! current-chord-root  chord-root)
  (reset! current-chord-name  chord-name)
  (reset! current-nr-notes    mouseNrNotes)
  (reset! current-length      length)
  ;;
  ;; (println chord-root chord-name)
  ;; (println mouseNrNotes "notes in" length "beats")
  ;; #(q/redraw)
  ;; (draw)
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

(def location  (atom nil))
(def current-chord-root (atom nil))
(def current-chord-name (atom nil))
(def current-nr-notes   (atom nil))
(def current-length     (atom nil))

(defn setup []
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 200)
  (q/frame-rate 30))                 ;; Set the background colour to

(defn draw []
  (q/background 100)
  (q/text (clojure.string/join ["mouseX = " (str (mouseX))]) 10 20)
  (q/text (clojure.string/join ["mouseY = " (str (mouseY))]) 10 40)
  (q/text (clojure.string/join ["current chord = " @current-chord-root @current-chord-name]) 10 60)
  (q/text (clojure.string/join [@current-nr-notes " notes in " @current-length " beats"]) 10 80)
  (q/text (clojure.string/join ["note-length = " (/ @current-length @current-nr-notes)]) 10 100)
  ;; (def normal-note (/ (int (* 36 (mouseX) )) 27))
  (def normal-note (/ 2 (int (max 1 (* 24 (mouseX))))))
  (q/text (clojure.string/join ["normal note-length = " normal-note]) 10 120))

(q/defsketch strum                  ;; Define a new sketch named example
  :title "Oh so much strumming"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [300 300]                     ;; You struggle to beat the golden ratio
  :setup #(q/background 100)
  ;; :settings q/no-loop
)
