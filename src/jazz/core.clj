(ns jazz.core)
(use 'overtone.core)
(connect-external-server)
(use 'overtone.inst.sampled-piano)

(def piano sampled-piano)
(piano)

;; this is one possible implementation of play-chord,
;; it was not shown in the video
(defn play-chord [a-chord]
  (doseq [note a-chord] (piano note)))

(defonce metro (metronome 120))
(metro)
(metro (+ 1 (metro)))

(defn my-strum
  "strum trough"
  [pTime chord-root chord-name inversion nrNotes length]
  ;; (arpeggio pTime (invert-chord(chord chord-root chord-name inversion) 12 ))

  (def theChord (chord chord-root chord-name inversion))

  (dotimes [i  nrNotes]
    (at (metro (+ pTime (/ (* length i) nrNotes) (metro)))
        (piano (first (invert-chord theChord  (+ 4 i) )))))
  )


(play-song)
(defn play-song
  "play trough the whole song"
  []
  (dotimes my-strum)
  )



(defn chordsA []
  (my-strum 0  :C2 :major  0 3 3/3)
  (my-strum 4  :C2 :minor  0 3 1)
  (my-strum 8  :A1 :minor7 0 4 4/3)
  (my-strum 12 :F1 :major  0 5 5/3)
  (apply-at (metro (+ 15 (metro))) chordsB []))
(chordsA)
(stop)

(play-chord 12 (:F3 :major) 0)
;; (defn chordsA [m beat-num]
;;   (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
;;   (at (m (+ 4 beat-num)) (play-chord (chord :C4 :minor)))
;;   (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor7)))
;;   (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
;;   (apply-at (m (+ 16 beat-num)) chordsB m (+ 16 beat-num) [])
;;   )

(defn chordsB []
  (my-strum 0   :A1 :minor 0 5 5/3)
  (my-strum 4   :G1 :minor 0 5 5/3)
  (my-strum 8   :F1 :minor 0 5 5/3)
  (my-strum 12  :G1 :minor 0 5 5/3)
  (apply-at (metro (+ 15 (metro))) chordsC []))

(defn chordsC []
  (my-strum 0  :C2 :m9     0 10 10/3)
  (my-strum 4  :C2 :minor  0 7 7/3)
  (my-strum 8  :A1 :dim    0 10 10/3)
  (my-strum 12 :F1 :major  0 7 7/3)
  (apply-at (metro (+ 15 (metro))) chordsD []))

(defn chordsD []
  (my-strum 0  :A1 :minor 0 12 4)
  (my-strum 4  :G1 :minor 0 10 10/3)
  (my-strum 8  :F1 :minor 0 12 4)
  (my-strum 12 :G1 :minor 0 7 7/3)
  (apply-at (metro (+ 15 (metro))) chordsE []))

(defn chordsE []
  (my-strum 0  :C2 :m9    0 12 4)
  (my-strum 4  :C2 :minor 0 12 4)
  (my-strum 8  :A1 :dim   1 12 4)
  (my-strum 12 :F1 :major 2 12 4)
  (apply-at (metro (+ 15 (metro))) chordsF []))

(defn chordsF []
  (my-strum 0  :A1 :minor 1 12 4)
  (my-strum 4  :G1 :minor 0 12 4)
  (my-strum 8  :F1 :minor 0 12 4)
  (my-strum 12 :F1 :dim   1 12 4)
  (apply-at (metro (+ 15 (metro))) chordsC [])
  )

(defn chordsEnd []
  (my-strum 0  beat-num)) (play-chord (chord :A3 :minor)))
  (my-strum 4  beat-num)) (play-chord (chord :G3 :minor)))
  (my-strum 8  beat-num)) (play-chord (chord :F3 :minor)))
  ;; (apply-at (m (+ 16 beat-num)) chordsA m (+ 16 beat-num) [])
)

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
