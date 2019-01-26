(ns fabric-patterns.core
  (:import
    [java.awt.image BufferedImage]
    [javax.swing JFrame WindowConstants]
    [java.awt Graphics Dimension Graphics2D RenderingHints Color BasicStroke]
    [java.awt.geom Ellipse2D$Float]))


(defn create-image [name width height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        graphics (.getGraphics image)
        frame (proxy [JFrame] []
                (paint [^Graphics graphics]
                  (let [y (- (.getHeight (.getSize this))
                             (.getHeight (.getContentPane this)))]
                    (.drawImage graphics image 0 y this))))]

    (.setRenderingHint ^Graphics2D graphics RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)

    (doto ^JFrame frame
      (.setTitle name)
      (-> .getContentPane (.setPreferredSize (Dimension. (.getWidth image nil) (.getHeight image nil))))
      (.setDefaultCloseOperation WindowConstants/HIDE_ON_CLOSE)
      .pack
      (.setVisible true))

    {:image image
     :width (.getWidth image nil)
     :height (.getHeight image nil)
     :graphics graphics
     :frame frame
     :repaint #(.repaint frame)
     ;:contains? (fn [x y]
     ;             (and (< 0 x width)
     ;                  (< 0 y height)))
     }))

(defn new-colour
  ([r g b]
   (Color. r g b))
  ([r g b a]
   (Color. r g b a)))

(defn clear [{:keys [graphics width height repaint]} colour]
  (.setColor graphics colour)
  (.fillRect graphics 0 0 width height)
  (repaint))

(defn circle [{:keys [graphics repaint]} x y r colour width]
  (.setStroke graphics (BasicStroke. width))
  (.setColor graphics colour)
  (.drawOval graphics (- x r) (- y r) (* 2 r) (* 2 r))
  (repaint))

(defn arc [{:keys [graphics repaint]} x y r start length colour width]
  (.setStroke graphics (BasicStroke. width BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  (.setColor graphics colour)
  ;(.drawOval graphics (- x r) (- y r) (* 2 r) (* 2 r))
  (.drawArc graphics (- x r) (- y r) (* 2 r) (* 2 r) start length)
  (repaint))


(defmacro with-clip [graphics clip & body]
  `(let [previous-clip# (.getClip ~graphics)]
     (try
       (.setClip ~graphics ~clip)
       ~@body
       (finally
         (.setClip ~graphics previous-clip#)))))

(defn blob [{:keys [graphics repaint] :as image} x y radius colour]
  (with-clip graphics (Ellipse2D$Float. (- x radius) (- y radius) (* 2 radius) (* 2 radius))
    (let [highlight-offset 0.3]
      (doseq [i (range 0 1 0.01)]
        (let [ring-radius (* i radius (+ 1.2 highlight-offset))
              light-factor (- 1.5 i)
              light-bump (* 50 (- 1.5 i))
              re-light (fn [v] (int (min 255 (* (+ v light-bump) light-factor))))]
          (circle image
                  (+ x (* radius highlight-offset))
                  (- y (* radius highlight-offset))
                  ring-radius
                  (Color. (re-light (.getRed colour))
                          (re-light (.getGreen colour))
                          (re-light (.getBlue colour)))
                  2)))))
  (repaint))

(def sample (create-image "Sample" 1000 1000))

(def green (new-colour 100 255 70))
(def pink (new-colour 255 25 255))
(def dark-pink (new-colour 150 0 50))
(def highlight (new-colour 255 255 255 127))
(def lowlight (new-colour 0 0 0 127))




(do

  (clear sample dark-pink)


  ;(circle sample 100 100 100 (new-colour 100 255 70) 5)

  (let [ring-count 50]
    (doseq [[y colour r] (map vector
                              (range 0 1000 (/ 1000 ring-count))
                              (cycle [pink green])
                              (map #(+ 60 (* 50 (Math/cos %)))
                                   (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) ring-count)))
                              )
            [x-offset y-offset] (map vector
                                     (range 0 1000 250)
                                     (cycle [0 500]))]

      (doseq [wrap-x [-1000 0 1000]
              wrap-y [-1000 0 1000]]
        ;(circle sample (+ wrap-x x-offset) (+ wrap-y y y-offset) r colour 3)
        (arc sample (+ wrap-x x-offset) (+ wrap-y y y-offset) r 0 180 colour 3)
        (arc sample (+ wrap-x x-offset) (+ wrap-y y y-offset) r 45 20 highlight 5)
        (arc sample (+ wrap-x x-offset) (+ wrap-y y y-offset) r 135 45 lowlight 3)


        )

      ))

  )

;(defn rgb->int [r g b]
;  (bit-or (bit-shift-left r 16)
;          (bit-shift-left g 8)
;          b))

;(defn draw
;  ([image x y r g b]
;   (draw image x y (rgb->int r g b)))
;  ([image x y rgb]
;   (when ((:contains? image) x y)
;     (.setRGB (:image image) x y rgb)
;     (.repaint (:frame image)))))


;(doseq [x (range 400) y (range 1000)]
;  (draw b x y 255 255 255))


; what are you doing?
; use the graphics object like you always used to
; use anti-aliasing

;(let [green (rgb->int 0 255 0)
;      pink (rgb->int 255 0 255)
;      circle-data {:radius (map #(+ 100 (* 75 (Math/sin %))) (range 0 100 0.2))
;                   :x-offset (repeat 120)
;                   :y-offset (range -100 1400 20)
;                   :colour (cycle [pink green])}
;      circles (take 60 (apply map (fn [& args] (zipmap (keys circle-data) args)) (vals circle-data)))]
;  (doseq [theta (range (/ (- Math/PI) 2) (/ Math/PI 2) 0.001)]
;    (let [x (Math/sin theta)
;          y (Math/cos theta)]
;      (doseq [{:keys [radius x-offset y-offset colour]} circles]
;        (doseq [r (range radius (+ radius 5))]
;          (doseq [[xa ya] [[0 0] [400 400] [800 0]]]
;            (draw b
;                  (+ xa x-offset (* x r))
;                  (+ ya y-offset (* y r))
;                  colour)))))))