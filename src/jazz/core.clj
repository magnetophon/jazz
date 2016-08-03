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
(defn arpeggio
  "strum trough"
  [pTime a-chord nr]
  ;; (at (m (+ (/ 1 nr) beat-num)) (piano (first (invert-chord a-chord 1))))
  (dotimes [i nr] (at (metro (+ pTime (/ i nr) (metro))) (piano (first (invert-chord a-chord i)))))
  ;; (dorun
  ;; (at (m (+ 0 beat-num)) (piano (nth a-chord 0)))
  ;; (at (m (+ 1/4 beat-num)) (piano (nth a-chord 1)))
  ;; (at (m (+ 2/4 beat-num)) (piano (nth a-chord 2)))
  ;; (at (m (+ 3/4 beat-num)) (piano (nth a-chord (3 % (length a-chord)))))
  ;; (apply-at (m (+ 1 beat-num)) arpeggio a-chord m(+ 1 beat-num) [] nr)
  ))
(println (metro))
(defn play-chord [pTime chord-root chord-name inversion] (arpeggio pTime (invert-chord(chord chord-root chord-name) inversion) 6 ))

(invert-chord(chord :F3 :major) 1)
(chord :F3 :major)
(chord [:F3 :major])


(defn mycho [chord-root chord-name] (chord chord-root chord-name) )
(mycho :F3 :major)

(:F3 :major)
(arpeggio 4 (chord :F3 :major) ) 6)
(stop)
(play-chord 0 :F3 :major 1)
(play-chord 0 (:F3 :major))
(play-chord (chord :A3 :minor))
(play-chord (chord :G3 :minor))
(play-chord (chord :F3 :minor))
(play-chord (chord :C4 :minor))

(defonce metro (metronome 120))
(metro)
(metro (+ 1 (metro)))
;; We can use recursion to keep playing the chord progression

(defn play-chords-at [pTime a-chord inversion]
  (play-chord pTime  (invert-chord(chord a-chord) inversion))
  )

(at (metro (+ 0 (metro))) (play-chord (chord :C4 :major)))

(defn chordsA []
  (play-chord 0  :C4 :major 0)
  (play-chord 4  :C4 :minor 0)
  (play-chord 8  :A3 :minor7 0)
  (play-chord 12 :F3 :major 0)
  (apply-at (metro (+ 16 (metro))) chordsB metro (+ 16 (metro)) [])
  )
(chordsA)

(play-chord 12 (:F3 :major) 0)
;; (defn chordsA [m beat-num]
;;   (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
;;   (at (m (+ 4 beat-num)) (play-chord (chord :C4 :minor)))
;;   (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor7)))
;;   (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
;;   (apply-at (m (+ 16 beat-num)) chordsB m (+ 16 beat-num) [])
;;   )

(defn chordsB [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :minor)))
  (at (m (+ 8 beat-num)) (play-chord (chord :F3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :G3 :minor)))
  (apply-at (m (+ 16 beat-num)) chordsC m (+ 16 beat-num) [])
  )

(defn chordsC [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :m9)))
  (at (m (+ 4 beat-num)) (play-chord (invert-chord (chord :C4 :minor) 0)))
  (at (m (+ 8 beat-num)) (play-chord (invert-chord (chord :A3 :dim) 0)))
  (at (m (+ 12 beat-num)) (play-chord (invert-chord  (chord :F3 :major) 0)))
  (apply-at (m (+ 16 beat-num)) chordsD m (+ 16 beat-num) [])
  )

(defn chordsD [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord  (invert-chord (chord :A3 :minor) 0)))
  (at (m (+ 4 beat-num)) (play-chord (invert-chord (chord :G3 :minor) 0)))
  (at (m (+ 8 beat-num)) (play-chord (invert-chord (chord :F3 :minor) 0)))
  (at (m (+ 12 beat-num)) (play-chord (invert-chord (chord :G3 :minor) 0)))
  (apply-at (m (+ 16 beat-num)) chordsE m (+ 16 beat-num) [])
  )

(defn chordsE [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :m9)))
  (at (m (+ 4 beat-num)) (play-chord (invert-chord (chord :C4 :minor) 1)))
  (at (m (+ 8 beat-num)) (play-chord (invert-chord (chord :A3 :dim) 1)))
  (at (m (+ 12 beat-num)) (play-chord (invert-chord  (chord :F3 :major) 2)))
  (apply-at (m (+ 16 beat-num)) chordsF m (+ 16 beat-num) [])
  )

(defn chordsF [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord  (invert-chord (chord :A3 :minor) 1)))
  (at (m (+ 4 beat-num)) (play-chord (invert-chord (chord :G3 :minor) 0)))
  (at (m (+ 8 beat-num)) (play-chord (invert-chord (chord :F3 :minor) 0)))
  (at (m (+ 12 beat-num)) (play-chord (invert-chord (chord :F3 :dim) 1)))
  (apply-at (m (+ 16 beat-num)) chordsC m (+ 16 beat-num) [])
  )

(defn chordsEnd [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :minor)))
  (at (m (+ 8 beat-num)) (play-chord (chord :F3 :minor)))
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
        timings (reductions (fn[t [_ d]] (+ t (tempo (* d 4)))) (now) melody)
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
