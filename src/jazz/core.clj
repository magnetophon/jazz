(ns jazz.core
  (require [quil.core :as q]))

(use 'overtone.core)
(connect-external-server)
(use 'overtone.inst.sampled-piano)
(import java.awt.Toolkit)

(def piano sampled-piano)
(piano)

(defn play-chord [a-chord]
  (doseq [note a-chord] (piano note)))

(play-chord (chord :C4 :major))

(defonce metro (metronome 120))
(metro)

(def location  (atom nil))

(defn global-state
  "keep track of the location in the song"
  []
  (my-strum (:chord-data (nth song @location)))
  (let [wait-time (:chord-length (nth song @location))]
    (apply-by (metro (+ wait-time (metro))) global-state [])
    (reset! location (mod (+ 1 @location) (count song)))))

(reset! location 0)
(println @location)

(global-state)
(stop)

(def song
  [{:chord-length 4
    :chord-data [:C2 :major  0 3 3/3]}
   {:chord-length 4
    :chord-data [:C2 :minor  0 3 1]}
   {:chord-length 4
    :chord-data [:A1 :minor7 0 4 4/3]}
   {:chord-length 4
    :chord-data [:F1 :major  0 5 5/3]}

   {:chord-length 4
    :chord-data [:A1 :minor 0 5 5/3]}
   {:chord-length 4
    :chord-data [:G1 :minor 0 5 5/3]}
   {:chord-length 4
    :chord-data [:F1 :minor 0 5 5/3]}
   {:chord-length 4
    :chord-data [:G1 :minor 0 5 5/3]}

   {:chord-length 4
    :chord-data [:C2 :m9     0 10 10/3]}
   {:chord-length 4
    :chord-data [:C2 :minor  0 7 7/3]}
   {:chord-length 4
    :chord-data [:A1 :dim    0 10 10/3]}
   {:chord-length 4
    :chord-data [:F1 :major  0 7 7/3]}

   {:chord-length 4
    :chord-data [:A1 :minor 0 12 4]}
   {:chord-length 4
    :chord-data [:G1 :minor 0 10 10/3]}
   {:chord-length 4
    :chord-data [:F1 :minor 0 12 4]}
   {:chord-length 4
    :chord-data [:G1 :minor 0 7 7/3]}

   {:chord-length 4
    :chord-data [:C2 :m9    0 12 4]}
   {:chord-length 4
    :chord-data [:C2 :minor 0 7 7/3]}
   {:chord-length 4
    :chord-data [:A1 :dim   1 12 4]}
   {:chord-length 4
    :chord-data [:F1 :major 2 7 7/3]}

   {:chord-length 4
    :chord-data [:A1 :minor 1 12 4]}
   {:chord-length 4
    :chord-data [:G1 :minor 0 12 4]}
   {:chord-length 4
    :chord-data [:F1 :minor 0 12 4]}
   {:chord-length 4
    :chord-data [:F1 :dim   1 7 7/3]}])

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
  [[chord-root chord-name inversion nrNotes length]]

  (def theChord (chord chord-root chord-name inversion))
  (def mouseNrNotes (int (max 1 (* 4 (mouseX) nrNotes))))
  (def transposeNr (int (* 8 (mouseY))))

  (dotimes [i   mouseNrNotes]
    (at (metro (+ (/ (* length i)  mouseNrNotes) (metro)))
        (piano (first (invert-chord theChord (+  transposeNr (mod i 24))))))))

(defn setup []
  (q/frame-rate 1)                    ;; Set framerate to 1 FPS
  (q/background 200)
  (q/frame-rate 30))                 ;; Set the background colour to

(str (take 2 (:chord-data (nth song @location))))
(take 2 (:chord-data (nth song @location)))
(take 2 (:chord-data (nth song @location)))

(mod (- @location 1) (count song))

(defn draw []
  (q/background 100)
  (q/text (clojure.string/join ["mouseX = " (str (mouseX))]) 10 20)
  (q/text (clojure.string/join ["mouseY = " (str (mouseY))]) 10 40)

  (let [
        chord-data  (:chord-data (nth song (mod (- @location 1) (count song))))
        current-nr-notes (int (max 1 (* 4 (mouseX) (get chord-data  3))))
        current-length (get chord-data  4)
        ]
    (q/text (clojure.string/join ["current chord = " (str (get chord-data  0)) (str (get chord-data  1))]) 10 60)
    (q/text (clojure.string/join [(str current-nr-notes ) " notes in " (str current-length) " beats"]) 10 80)
  (q/text (clojure.string/join ["note-length = " (/ current-length current-nr-notes)]) 10 100)
  ;; (def normal-note (/ (int (* 36 (mouseX) )) 27))
  (def normal-note (/ 2 (int (max 1 (* 24 (mouseX))))))
  (q/text (clojure.string/join ["normal note-length = " normal-note]) 10 120)))

(q/defsketch strum                  ;; Define a new sketch named example
  :title "Oh so much strumming"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [300 300]                     ;; You struggle to beat the golden ratio
  :setup #(q/background 100)
  ;; :settings q/no-loop
)
