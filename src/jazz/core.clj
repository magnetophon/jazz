(ns jazz.core)
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

(Toolkit/getProperty)

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

(atBeat 0 my-strum  [:F1 :major 0 5 5/3])
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
  (atBeat 15 chordsC []))

;; (defn chordsEnd []
;;   (my-strum 0  beat-num)) (play-chord (chord :A3 :minor)))
;;   (my-strum 4  beat-num)) (play-chord (chord :G3 :minor)))
;;   (my-strum 8  beat-num)) (play-chord (chord :F3 :minor)))
;;   ;; (apply-at (m (+ 16 beat-num)) chordsA m (+ 16 beat-num) [])
;; )

(chordsA  metro (metro))
(chordsEnd  metro (metro))

(scale :C3 :major)
(stop)

(defn bpm
  "Higher order function. Returns another fn to compute the
  time offset in milliseconds for a beat at given `tempo`."
  [tempo]
  (fn [beat] (* (/ beat tempo) 60000)))

;; Next we encode a well know melody in an abstract manner
;; using musical degrees. Read this if you're unfamiliar with the concept:
;; http://en.wikipedia.org/wiki/Degree_(music)
;; Also highly recommended is this book for further contextualization:
;; http://www.amazon.co.uk/Quadrivium-Number-Geometry-Music-Heaven/dp/190715504X

(def melody
  "The Happy Birthday melody in scale-less musical degrees.
  The keyword :_ identifies a pause.
  Note durations are in bar measure (i.e. 1/4 = quarter note)."
  [;; Hap    py        birth      day        to        you
   [:v- 1/8] [:v- 1/8] [:vi- 1/4] [:v- 1/4]  [:i 1/4]  [:vii- 1/2]
   ;; Hap    py        birth      day        to        you
   [:v- 1/8] [:v- 1/8] [:vi- 1/4] [:v- 1/4]  [:ii 1/4] [:i 1/2]
   ;; Hap    py        birth      day        dear      Ri       car         do
   [:v- 1/8] [:v- 1/8] [:v 1/4]   [:iii 1/4] [:i 1/8]  [:i 1/8] [:vii- 1/4] [:vi- 1/4] [:_ 1/4]
   ;; Hap    py        birth      day        to        you
   [:iv 1/8] [:iv 1/8] [:iii 1/4] [:i 1/4]   [:ii 1/4] [:i 1/2]])

(defn play-tune
  "Takes an instrument, a sequence of notes and tempo (in bpm).
  Plays notes in separate thread."
  [inst bpm# root scale melody]
  (let [tempo (bpm bpm#)
        timings (reductions (fn [t [_ d]] (+ t (tempo (* d 4)))) (now) melody)
        root (note root)
        play-note (fn [timing [degree dur]]
                    (when-not (= :_ degree)
                      (at timing (inst (+ root (degree->interval degree scale))))))]
    (dorun (map play-note timings melody))))

(defn repeat-notes
  "Takes a melody sequence and repeats each note `n` times,
  with 1/n of its original duration. Returns new melody sequence."
  [n melody]
  (mapcat (fn [[deg dur]] (repeat n [deg (/ dur n)])) melody))

(defn arpeggiate
  "Similar to arpeggiate fn in the ex01_phrasestudy ns, but working with degrees
  instead of absolute notes and also supporting pauses. Since degrees are expressed
  as Roman numeral keywords (and not as number), we append `+` as suffix to indicate
  a note of the same degree only one octave higher."
  [n melody]
  (mapcat
   (fn [[deg dur]]
     (if-not (= :_ deg)
       (take n (cycle [[deg (/ dur n)] [(keyword (str (name deg) "+")) (/ dur n)]]))
       [[deg dur]]))
   melody))

;; (comment
  ;; Play the original Happy Birthday tune in F4 major
(play-tune piano 120 :f4 :major melody)
  ;; The following experiments go ever further away from the original melody...
  ;; All this is only achieved through manipulating the original sequence
  ;; and/or choosing unusual scales. Since we only specified the melody in
  ;; degrees it will always be "in tune", regardless of scale changes
(play-tune piano 120 :f4 :egyptian melody)
(play-tune piano 120 :c4 :major (repeat-notes 3 melody))
(play-tune piano 120 :c4 :major (arpeggiate 2 melody))
(play-tune piano  60 :c4 :egyptian (arpeggiate 3 melody)) ; my favourite!
(play-tune piano  60 :c4 :diminished (arpeggiate 4 (reverse melody)))
  ;; )
(dorun (map-indexed #(at (+ (now) (* % 200)) (piano (+ 60 %2))) [0 2 7 12 24 19 14 12]))
