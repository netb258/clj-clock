(ns clock.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(def WIDTH 500)
(def HEIGHT 500)

(def TICK-WIDTH 5)
(def TICK-HEIGHT 80)

;; The ticks for hrs, min and sec are going to be trianges of 3 points, like this: [[10 25] [50 65] [80 95]]
(def CENTER-X (/ WIDTH 2))
(def CENTER-Y (- (/ HEIGHT 2) TICK-HEIGHT)) ;; The center is at the bottom of the triangles.

(def CLOCK-CENTER [CENTER-X (+ CENTER-Y TICK-HEIGHT)])
(def START-POINT-1 [CENTER-X (- CENTER-Y TICK-HEIGHT)])
(def START-POINT-2 [(- CENTER-X TICK-WIDTH) (+ CENTER-Y TICK-HEIGHT)])
(def START-POINT-3 [(+ CENTER-X TICK-WIDTH) (+ CENTER-Y TICK-HEIGHT)])

;; The ticks for hrs, min and sec are represented as trianges of 3 points, like this: [[10 25] [50 65] [80 95]]
(def CLOCK
  ;;NOTE: The hours triangle should be a bit shorter than the rest.
  {:hours [[(first START-POINT-1) (+ (second START-POINT-1) 30)] START-POINT-2 START-POINT-3]
   :minutes [START-POINT-1 START-POINT-2 START-POINT-3]
   :seconds [START-POINT-1 START-POINT-2 START-POINT-3]})

(defn rotate-triangle
  "Rotates the triangle at a certain angle.
  The triangle is a vector of 3 (x,y) coordinates like this:
  [[320 220] [310 260] [330 260]]
  The center is a point like this: [50 50].
  The angle is an int between 1 and 360.
  Returns the triangle vector with transformed coordinates like this:
  (rotate-triangle [[320 220] [310 260] [330 260]] CLOCK-CENTER 90) => [[220.0 320.0] [260.0 310.0] [260.0 330.0]]"
  [[[x1 y1] [x2 y2] [x3 y3]] [center-x center-y] angle]
  (let [angle-radians (Math/toRadians angle)]
    [[(Math/abs
       (- (* (- x1 center-x) (Math/cos angle-radians))
          (+ (* (- y1 center-y) (Math/sin angle-radians)) center-x)))
      (Math/abs
       (+ (* (- x1 center-x) (Math/sin angle-radians))
          (+ (* (- y1 center-y) (Math/cos angle-radians)) center-y)))]
     [(Math/abs
       (- (* (- x2 center-x) (Math/cos angle-radians))
          (+ (* (- y2 center-y) (Math/sin angle-radians)) center-x)))
      (Math/abs
       (+ (* (- x2 center-x) (Math/sin angle-radians))
          (+ (* (- y2 center-y) (Math/cos angle-radians)) center-y)))]
     [(Math/abs
       (- (* (- x3 center-x) (Math/cos angle-radians))
          (+ (* (- y3 center-y) (Math/sin angle-radians)) center-x)))
      (Math/abs
       (+ (* (- x3 center-x) (Math/sin angle-radians))
          (+ (* (- y3 center-y) (Math/cos angle-radians)) center-y)))]]))

(defn set-min-sec
  [secs-triangle secs-as-int]
  ;; NOTE: Partitioning the 360 degrees into 6 will give us exactly 60 equal pieces (60 * 6 = 360).
  (let [secs-as-angles (reverse (mapv last (partition 6 (range 0 360))))]
    (rotate-triangle secs-triangle CLOCK-CENTER (nth secs-as-angles secs-as-int))))

(defn set-hour
  [hrs-triangle hrs-as-int]
  ;; NOTE: Partitioning the 360 degrees into 30 will give us exactly 12 equal pieces (30 * 12 = 360).
  (let [hrs-as-angles (reverse (mapv last (partition 30 (range 0 360))))
        hrs-as-int (mod hrs-as-int 12)]
    (rotate-triangle hrs-triangle CLOCK-CENTER (nth hrs-as-angles hrs-as-int))))

(defn get-hour-points
  "Returns the 12 points on the clock where the hours lay.
  Example:
  (get-hour-points) => '([332 112] [389 172] [409 252] [387 332] [327 389] [247 409] [167 387] [110 327] [90 247] [112 167] [172 110] [50 50])"
  []
  (->> (range 0 12)
       (map #(set-hour (:minutes CLOCK) %)) ;; Rotate the triangle in every possible 12 ways.
       (map first))) ;; Get only the first points of the triangle (namely the tip of the triangle).

(defn get-min-sec-points
  "Returns the 60 points on the clock where the minutes and seconds lay."
  []
  (->> (range 0 60)
       (map #(set-min-sec (:minutes CLOCK) %)) ;; Rotate the minutes triangle in every possible 60 ways.
       (map first))) ;; Get only the first points of the triangle (namely the tip of the triangle).

(defn get-hour-numbers-points
  "Returns the 12 points on the clock where we should print the hour numbers from 1 to 12."
  []
  (->> (range 0 12)
        ;; Rotate the triangle in every possible 12 ways (NOTE: We are deliberately using a longer triangle here).
       (map #(set-hour [[(first START-POINT-1) (- (second START-POINT-1) 20)] START-POINT-2 START-POINT-3] %))
       (map first)
       (reverse)))

(defn get-min-sec-numbers-points
  "Returns the 12 points on the clock where we should print the min/sec numbers from 0 to 55."
  []
  (->> (range 0 60)
        ;; Rotate the triangle in every possible 12 ways (NOTE: We are deliberately using a shorter triangle here).
       (map #(set-min-sec [[(first START-POINT-1) (+ (second START-POINT-1) 20)] START-POINT-2 START-POINT-3] %))
       (map first)
       (take-nth 5) ;; We only need the ones for 0, 5, 10, 15, 20 ...
       (reverse)))

(defn setup []
  (q/frame-rate 60)
  (q/stroke 0)
  (q/stroke-weight 0)
  (q/background 20 20 20)
  ;; Here we return the initial state that the other function(update-state, draw-state!, etc...) will work with.
  CLOCK)

(defn draw-state!
  [clock-state]
  (q/background 35) ;; Clear the canvas before each draw.
  (q/text-align :center)
  (q/text-size 11)

  ;; The minutes tick and points should be white:
  (q/stroke 255 255 255)
  (q/fill  255 255 255)
  (doall (map #(q/ellipse (first %) (second %) 3 3) (get-min-sec-points)))
  (apply q/triangle (flatten (:minutes clock-state)))
  (let [min-sec-num-points (get-min-sec-numbers-points)]
    (doall (->> (range 0 60)
                (take-nth 5)
                (reverse)
                (map-indexed (fn [idx num]
                               (q/text
                                (str num)
                                (first (nth min-sec-num-points idx))
                                (second (nth min-sec-num-points idx))))))))

  ;; The seconds tick should be drawn in red:
  (q/stroke 255 100 150)
  (q/fill 255 100 150)
  (apply q/triangle (flatten (:seconds clock-state)))

  ;; The hours tick, points and numbers should be blue:
  (q/stroke 150 100 255)
  (q/fill 150 100 255)
  (q/text-size 15)
  (apply q/triangle (flatten (:hours clock-state)))
  (let [hour-points (get-hour-points)
        hour-num-points (get-hour-numbers-points)]
    (doall (map #(q/ellipse (first %) (second %) 10 10) hour-points))
    ;; This sucks, make a function that gets the hour numbers points.
    (doall (->> (cons 12 (rest (range 0 12))) ;; NOTE: The nums should be 1 - 12, not 0 - 11, so we need a slight fix.
                (reverse)
                (map-indexed (fn [idx num]
                               (q/text
                                (str num)
                                (first (nth hour-num-points idx))
                                (second (nth hour-num-points idx))))))))

  ;; One circle in the center just for aesthetics:
  (q/ellipse (/ WIDTH 2) (/ HEIGHT 2) 20 20))

(defn update-state [clock-state]
  (let [current-date (java.util.Date.)]
    {:hours (set-hour (:hours CLOCK) (.getHours current-date))
     :minutes (set-min-sec (:minutes CLOCK) (.getMinutes current-date))
     :seconds (set-min-sec (:seconds CLOCK) (.getSeconds current-date))}))

(defn -main []
  (q/defsketch clock
    :title "Clock"
    :settings #(q/smooth 2)
    :renderer :java2d
    :setup setup
    :draw draw-state!
    :update update-state
    :size [WIDTH HEIGHT]
    :middleware [m/fun-mode]))
