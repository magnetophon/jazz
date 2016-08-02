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

(play-chord (chord :F3 :major))
(play-chord (chord :A3 :minor))
(play-chord (chord :G3 :minor))
(play-chord (chord :F3 :minor))
(play-chord (chord :C4 :minor))

(defonce metro (metronome 120))
(metro)
;; We can use recursion to keep playing the chord progression
(defn chordsA [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :C4 :minor)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor7)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  (apply-at (m (+ 16 beat-num)) chordsB m (+ 16 beat-num) [])
  )

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

(def scale-degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :_ :vii :_])
(def pitches (degrees->pitches scale-degrees :dorian :C4))

(defn play [time notes sep]
  (let [note (first notes)]
    (when note
      (at time (saw (midi->hz note))))
    (let [next-time (+ time sep)]
      (apply-at next-time play [next-time (rest notes) sep]))))


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
