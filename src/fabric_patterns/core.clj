(ns fabric-patterns.core
  (:require
    [clojure.java.io :as io])
  (:import
    [java.awt.image BufferedImage RenderedImage]
    [javax.swing JFrame WindowConstants]
    [java.awt Graphics Dimension Graphics2D RenderingHints Color BasicStroke Polygon]
    [java.awt.geom Ellipse2D$Float]
    [javax.imageio ImageIO]
    [java.io File]))


(def TAU (* 2 Math/PI))

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

    {:image    image
     :width    (.getWidth image nil)
     :height   (.getHeight image nil)
     :graphics graphics
     :frame    frame
     :repaint  #(.repaint frame)
     ;:contains? (fn [x y]
     ;             (and (< 0 x width)
     ;                  (< 0 y height)))
     }))

(defn new-colour
  ([r g b]
   (Color. r g b))
  ([r g b a]
   (Color. r g b a)))

(defn doubler [shape]
  [shape
   (assoc shape :x 150)])

(defn clear [colour]
  {:op     :clear
   :colour colour
   :z      Integer/MIN_VALUE})

(defn circle [x y z r style]
  {:op     :circle
   :x      x
   :y      y
   :z      z
   :r      r
   :style style})

(defn line [x1 y1 x2 y2 z style]
  {:op     :line
   :x1     x1
   :y1     y1
   :x2     x2
   :y2     y2
   :z      z
   :style style})

(defn arc [x y z r start length style]
  {:op     :arc
   :x      x
   :y      y
   :z      z
   :r      r
   :start  start
   :length length
   :style style})

(defn blob [x y z r colour alpha]
  {:op     :blob
   :x      x
   :y      y
   :z      z
   :r      r
   :colour colour
   :alpha  alpha})

(defn polygon [x-coords y-coords z style]
  {:op      :polygon
   ;:polygon (Polygon. x-coords y-coords (count x-coords))
   :xs x-coords
   :ys y-coords
   :z       z
   :style   style})

(defn regular-polygon [x y z number-of-sides size rotation style star?]
  (let [angles (map #(+ % rotation)
                    (if star?
                      (range 0 (* 2 TAU) (* 2 (/ TAU number-of-sides)))
                      (range 0 TAU (/ TAU number-of-sides))))
        x-coords (map #(+ x (* size (Math/sin %))) angles)
        y-coords (map #(+ y (* size (Math/cos %))) angles)]
    (polygon x-coords y-coords z style)))

(defn wrap [instruction]
  (cond
    (and (:x instruction)
         (:y instruction))
    (for [wrap-x [-1000 0 1000]
          wrap-y [-1000 0 1000]]
      (-> instruction
          (update :x + wrap-x)
          (update :y + wrap-y)
          (update :z + wrap-x wrap-y)))

    :else
    instruction))

;(defn blob [{:keys [graphics repaint] :as image} x y radius colour]
;  (with-clip graphics (Ellipse2D$Float. (- x radius) (- y radius) (* 2 radius) (* 2 radius))
;    (let [highlight-offset 0.3]
;      (doseq [i (range 0 1 0.01)]
;        (let [ring-radius (* i radius (+ 1.2 highlight-offset))
;              light-factor (- 1.5 i)
;              light-bump (* 50 (- 1.5 i))
;              re-light (fn [v] (int (min 255 (* (+ v light-bump) light-factor))))]
;          (circle image
;                  (+ x (* radius highlight-offset))
;                  (- y (* radius highlight-offset))
;                  ring-radius
;                  (Color. (re-light (.getRed colour))
;                          (re-light (.getGreen colour))
;                          (re-light (.getBlue colour)))
;                  2)))))
;  (repaint))

(def green (new-colour 100 255 70))
(def pink (new-colour 255 25 255))
(def dark-pink (new-colour 150 0 50))
(def highlight (new-colour 255 255 255 127))
(def lowlight (new-colour 0 0 0 127))
(def black Color/BLACK)

(defn re-light [colour bump factor]
  (let [adjust (fn [v]
                 (int (max 0 (min 255 (* (+ v bump) factor)))))]
    (Color. ^Integer (adjust (.getRed colour))
            ^Integer (adjust (.getGreen colour))
            ^Integer (adjust (.getBlue colour)))))

(defn lighter [colour]
  (re-light colour 10 1.1))

(defn darker [colour]
  (re-light colour -10 0.9))

(defn lighter-or-darker [colour]
  (re-light colour (- (rand 20) 10) (+ 0.9 (rand 0.2))))

(defn much [f x]
  (f (f (f x))))


(defn stroke [colour width]
  {:stroke {:colour colour
            :width width}})

(defn fill [colour]
  {:fill {:colour colour}})

(defmacro do-stroke [graphics style stroke-expr]
  `(when (:stroke ~style)
     (.setStroke ~graphics (BasicStroke. (:width (:stroke ~style)) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
    (.setColor ~graphics (:colour (:stroke ~style)))
    ~stroke-expr))

(defmacro do-fill [graphics style fill-expr]
  `(when (:fill ~style)
     (.setColor ~graphics (:colour (:fill ~style)))
     ~fill-expr))

(defmacro styled [graphics style stroke-expr fill-expr]
  `(do
     (do-stroke ~graphics ~style ~stroke-expr)
     (do-fill ~graphics ~style ~fill-expr)))

(defmacro with-clip [graphics clip & body]
  `(let [previous-clip# (.getClip ~graphics)]
     (try
       (.setClip ~graphics ~clip)
       ~@body
       (finally
         (.setClip ~graphics previous-clip#)))))

(defmulti wrap (fn [instruction] (:op instruction)))

(defmethod wrap :default [instruction]
  (for [wrap-x [-1000 0 1000]
        wrap-y [-1000 0 1000]]
    (-> instruction
        (update :x + wrap-x)
        (update :y + wrap-y)
        (update :z + wrap-x wrap-y))))

(defmethod wrap :clear [instruction] instruction)

(defmethod wrap :line [instruction]
  (for [wrap-x [-1000 0 1000]
        wrap-y [-1000 0 1000]]
    (-> instruction
        (update :x1 + wrap-x)
        (update :y1 + wrap-y)
        (update :x2 + wrap-x)
        (update :y2 + wrap-y)
        (update :z + wrap-x wrap-y))))

(defmethod wrap :polygon [instruction]
  (for [wrap-x [-1000 0 1000]
        wrap-y [-1000 0 1000]]
    (-> instruction
        (update :xs #(map (partial + wrap-x) %))
        (update :ys #(map (partial + wrap-y) %))
        (update :z + wrap-x wrap-y))))

(defmulti render (fn [_image_ instruction] (:op instruction)))

(defmethod render :default [_iamge_ instruction]
  (throw (ex-info "unknown instruction" {:instruction instruction})))

(defmethod render :circle [{:keys [graphics repaint]} {:keys [x y r style]}]
  (styled graphics style
          (.drawOval graphics (- x r) (- y r) (* 2 r) (* 2 r))
          (.fillOval graphics (- x r) (- y r) (* 2 r) (* 2 r)))
  (repaint))

(defmethod render :arc [{:keys [graphics repaint]} {:keys [x y r start length style]}]
  (styled graphics style
          (.drawArc graphics (- x r) (- y r) (* 2 r) (* 2 r) start length)
          (.fillArc graphics (- x r) (- y r) (* 2 r) (* 2 r) start length))
  (repaint))

(defmethod render :clear [{:keys [graphics width height repaint]} {:keys [colour]}]
  (.setColor graphics colour)
  (.fillRect graphics 0 0 width height)
  (repaint))

(defmethod render :line [{:keys [graphics repaint]} {:keys [x1 y1 x2 y2 style]}]
  (styled graphics style
          (.drawLine graphics x1 y1 x2 y2)
          (.drawLine graphics x1 y1 x2 y2))
  (repaint))

(defmethod render :blob [{:keys [graphics repaint]} {:keys [x y r colour alpha]}]

  (let [image-size (int (* 2 r))
        temp-image (BufferedImage. image-size image-size BufferedImage/TYPE_INT_ARGB)
        temp-graphics ^Graphics2D (.getGraphics temp-image)
        clip-area (Ellipse2D$Float. 0 0 image-size image-size)]

    (with-clip temp-graphics clip-area
      (let [highlight-offset 0.3]
        (doseq [i (range 0 1 0.005)]
          (let [ring-radius (* i r (+ 1.2 highlight-offset))
                light-factor (- 1.5 i)
                light-bump (* 50 (- 1.5 i))
                size (* 2 ring-radius)]
            (.setStroke temp-graphics (BasicStroke. 2))
            (.setColor temp-graphics (re-light colour light-bump light-factor))
            (.drawOval temp-graphics
                       (- (+ r (* r highlight-offset)) ring-radius)
                       (- (- r (* r highlight-offset)) ring-radius) size size)))))

    (doseq [i (range image-size)
            j (range image-size)
            :when (.contains clip-area i j)] ; TODO: limit to within radius!
      (try
        (let [colour (Color. (.getRGB temp-image i j))
              new-colour (Color. (.getRed colour)
                                 (.getGreen colour)
                                 (.getBlue colour)
                                 (min (.getAlpha colour) alpha))]
          (.setRGB temp-image i j (.getRGB new-colour)))
        (catch Exception e
          (println image-size i j)
          (throw e)
          )))

    (.drawImage graphics temp-image (- x r) (- y r) image-size image-size nil)

    )

  ;(with-clip graphics (Ellipse2D$Float. (- x r) (- y r) (* 2 r) (* 2 r))
  ;  (let [highlight-offset 0.3]
  ;    (doseq [i (range 0 1 0.005)]
  ;      (let [ring-radius (* i r (+ 1.2 highlight-offset))
  ;            light-factor (- 1.5 i)
  ;            light-bump (* 50 (- 1.5 i))
  ;            size (* 2 ring-radius)]
  ;        (.setStroke graphics (BasicStroke. 2))
  ;        (.setColor graphics (re-light colour light-bump light-factor))
  ;        (.drawOval graphics
  ;                   (- (+ x (* r highlight-offset)) ring-radius)
  ;                   (- (- y (* r highlight-offset)) ring-radius) size size)))))
  (repaint))

(defmethod render :polygon [{:keys [graphics repaint]} {:keys [xs ys style]}]
  (let [polygon (Polygon. (int-array xs) (int-array ys) (count xs))]
    (styled graphics style
            (.drawPolygon graphics polygon)
            (.fillPolygon graphics polygon)))
  (repaint))

(defn render-all [image & instructions]
  (->> instructions
       flatten
       (map wrap)
       flatten
       (sort-by :z)
       (mapv #(render image %))
       count
       (str "Instructions rendered: ")
       println))

(defn flower [x y z r rotation colour]
  #_(for [[x-offset y-offset] (map vector
                                 (range 0 1000 250)
                                 (cycle [0 500]))]
    (let [points (for [i (range 0 1 0.001)]
                   (let [theta (+ (* i 4 Math/PI))
                         radius (* r (Math/sin (* theta 12/8)))]
                     [(* radius (Math/sin (+ rotation theta)))
                      (* radius (Math/cos (+ rotation theta)))]))
          xs (mapv #(+ x (first %) x-offset) points)
          ys (mapv #(+ y (second %) y-offset) points)]
      (polygon xs ys z (fill colour))))
  (let [points (for [i (range 0 1 0.001)]
                 (let [theta (+ (* i 4 Math/PI))
                       radius (* r (Math/sin (* theta 12/8)))]
                   [(* radius (Math/sin (+ rotation theta)))
                    (* radius (Math/cos (+ rotation theta)))]))
        xs (mapv #(+ x (first %)) points)
        ys (mapv #(+ y (second %)) points)]
    (polygon xs ys z (fill colour))))

(defn heart [x y z size rotation colour]
  (let [radius (* size 0.6)
        separation (* size 0.5)
        width (* size 1.0)
        height (* size 1.2)
        drop (* size 0.32)]
    [(circle (+ x separation) y z radius (fill colour))
     (circle (- x separation) y z radius (fill colour))
     (polygon [(- x width) (+ x width) x]
              [(+ y drop) (+ y drop) (+ y drop height)]
              z (fill colour))]))

(def offset-to-vines 125)

(defn draw-pattern []
  (let [sample (create-image "Sample" 1000 1000)]
    (render-all sample

      [(clear dark-pink)]

      ; cages
      (let [ring-count 50]
        (for [[y r] (map vector
                         (range 0 1000 (/ 1000 ring-count))
                         (map #(+ 60 (* 50 (Math/cos %)))
                              (range 0 (* 2 Math/PI) (/ (* 2 Math/PI) ring-count))))
              [x-offset y-offset] (map vector
                                       (range 0 1000 250)
                                       (cycle [0 500]))]
          (let [colour (much lighter-or-darker pink)]
            [(arc x-offset (+ y y-offset) 10 r 0 180 (stroke (->> colour darker darker darker) 1))
             (arc x-offset (+ y y-offset) 10 r 10 150 (stroke (->> colour darker darker) 2))
             (arc x-offset (+ y y-offset) 10 r 20 120 (stroke (->> colour darker) 3))
             (arc x-offset (+ y y-offset) 10 r 30 90 (stroke (->> colour) 4))
             (arc x-offset (+ y y-offset) 10 r 40 60 (stroke (->> colour (much lighter)) 5))
             (arc x-offset (+ y y-offset) 10 r 50 30 (stroke (->> colour (much lighter) (much lighter)) 6))
             (arc x-offset (+ y y-offset) 10 r 60 10 (stroke (->> colour (much lighter) (much lighter) (much lighter)) 7))
             ;(wrap (arc x-offset (+ y y-offset) 10 r 0 180 colour 3))
             ;(wrap (arc x-offset (+ y y-offset) 10 r 45 20 highlight 5))
             ;(wrap (arc x-offset (+ y y-offset) 10 r 135 45 lowlight 3))
             ])))

      ; pearls
      (for [[y r] (map vector
                       [-260 -160 0 160 260]
                       [10 40 75 40 10])
            [x-offset y-offset] (map vector
                                     (range 0 1000 250)
                                     (cycle [0 500]))]
        [(blob x-offset (+ y y-offset) -2001 r green 255)
         (circle x-offset (+ y y-offset) -2000 r (stroke black 1))])


      ; bubbles
      #_(for [[i even] (map vector
                            (range 0 1 0.01)
                            (cycle [true false]))
              [x-offset y-offset] (map vector
                                       (range 0 1000 250)
                                       (cycle [0 500]))]

          (let [y (+ (* i 1000) y-offset)
                factor (+ 25 (* 25 (Math/cos (* i 2 Math/PI))))

                control 0.5
                random (- 1 control)

                control-x (* 1 factor (if even 1 -1))
                random-x (* factor (- 1 (rand 2)))

                ;control-radius (* 1 factor)
                radius (+ 1 (* (rand 0.9) factor))

                ;radius (+ (* control-radius 0.1)
                ;          (* random-radius 0.9))
                ;x (+ (* (- (rand 50) 25) factor) x-offset)
                x (+ x-offset
                     (* control-x control)
                     (* random-x random))


                z (+ -2000 (rand-int 100))]
            [(wrap (circle x y (- z 4000) radius (fill (new-colour 200 255 200))))
             (wrap (blob x y z radius (new-colour 80 255 80) 100))
             (wrap (circle x y z radius (stroke (new-colour 0 100 0) 1)))]

            )
          )

      ; vines
      (for [[x-offset y-offset] (map vector
                                     (range 0 1000 250)
                                     (cycle [0 500]))
            [[y1 xb1 xc1 xd1 zb1 zc1 zd1]
             [y2 xb2 xc2 xd2 _ _ _]]
            (partition 2 1
              (for [i (take 65 (range 0 2 (/ 1 64)))]
                (let [y (* i 1000)
                      theta-a (* i 2 Math/PI)
                      theta-b (+ (* i 8 Math/PI) (* Math/PI 0/3))
                      theta-c (+ (* i 8 Math/PI) (* Math/PI 1/3))
                      theta-d (+ (* i 8 Math/PI) (* Math/PI 2/3))
                      x (+ offset-to-vines (* 50 (Math/cos theta-a)))
                      xb (+ x (* 20 (Math/sin theta-b)))
                      xc (+ x (* 20 (Math/sin theta-c)))
                      xd (+ x (* 20 (Math/sin theta-d)))
                      zb (* 20 (Math/cos theta-b))
                      zc (* 20 (Math/cos theta-c))
                      zd (* 20 (Math/cos theta-d))]
                  [y xb xc xd zb zc zd])))]

        [(line (+ xb1 x-offset) (+ y1 y-offset) (+ xb2 x-offset) (+ y2 y-offset) zb1 (stroke (darker green) 5))
         (line (+ xc1 x-offset) (+ y1 y-offset) (+ xc2 x-offset) (+ y2 y-offset) zc1 (stroke (much darker green) 5))
         (line (+ xd1 x-offset) (+ y1 y-offset) (+ xd2 x-offset) (+ y2 y-offset) zd1 (stroke (darker (darker green)) 5))]

        )

      ; leaves
      (for [i (range 0 1 (/ 1 16))
            [x-offset y-offset] (map vector
                                     (range 0 1000 250)
                                     (cycle [0 500]))]

        (let [y (+ (* i 1000) y-offset)
              theta (* i 2 Math/PI)
              x (+ offset-to-vines (* 50 (Math/cos theta)) x-offset (if (even? (int (* i 16)))
                                                                      20
                                                                      -20))
              leaf-colour (much lighter-or-darker (much lighter-or-darker (much lighter-or-darker (much lighter-or-darker green))))
              leaf-length 25]

          ; each leaf
          (let [leaf-points (for [n
                                  ;(range (- leaf-length) leaf-length 4)
                                  (range (- leaf-length) (inc leaf-length) 2)
                                  ;[-20 -15 -10 -5 0 5 10 15 20]
                                  ]

                              (let [xa (* n (Math/sin theta))
                                    ya (* n (Math/cos theta))
                                    leaf-width (+ 6 (* 6 (Math/cos (* (/ n leaf-length) Math/PI))))
                                    xb (* leaf-width (Math/cos theta))
                                    yb (- (* leaf-width (Math/sin theta)))

                                    x1 (+ x xa xb)
                                    y1 (+ y ya yb)
                                    x2 (- (+ x xa) xb)
                                    y2 (- (+ y ya) yb)]

                                [[x1 y1] [x2 y2]]

                                ;[(wrap (line x1 y1 x2 y2 0 leaf-colour 1))]

                                ))
                half-one (map first leaf-points)
                half-two (reverse (map second leaf-points))
                x-coords (concat (map first half-one) (map first half-two))
                y-coords (concat (map second half-one) (map second half-two))]

            (polygon x-coords y-coords 100 (merge (fill leaf-colour) (stroke black 1)))

            )))

      ; flowers
      (for [i (range 0 1 (/ 1 5))]
        (let [y (* i 1000)
              theta (* i 2 Math/PI)
              x (+ offset-to-vines (* 50 (Math/cos theta)))
              rotations (iterate (partial + 0.5) (rand 10))
              sizes (iterate (partial * 0.9) 50)
              zs (iterate inc 1000)
              colour (new-colour 255 255 100 200)]
          (concat
            (for [[x-offset y-offset] (map vector
                                           (range 0 1000 250)
                                           (cycle [0 500]))]
              (circle (+ x x-offset) (+ y y-offset) 999 25 (fill pink)))
            [(flower x y (nth zs 0) (nth sizes 0) (nth rotations 0) colour)
             (flower x y (nth zs 1) (nth sizes 1) (nth rotations 1) colour)
             (flower x y (nth zs 2) (nth sizes 2) (nth rotations 2) colour)
             ]))))

    ; ImageIO.write(image, "png", new File("./" + filename + ".png"));
    (ImageIO/write ^RenderedImage (:image sample) "png" (io/file (str "sample-2.png")))))


(defn spit-image-to-file [image filename]
  (ImageIO/write ^RenderedImage (:image image) "png" (io/file (str filename ".png"))))


(comment


  (def example (create-image "Example" 1000 1000))

  (render-all example (clear green))

  (render-all example (circle 500 500 0 300 (fill pink)))

  (render-all example (circle 0 0 0 300 (fill pink)))

  (render-all example (flower 500 500 0 200 0 (new-colour 255 255 0)))

  (def TAU (* 2 Math/PI))
  (render-all example
              (for [angle (range 0 (* 30 TAU) 0.5)]
                (circle (+ 500 (* (+ 100 angle) (Math/sin angle)))
                        (+ 500 (* (+ 100 angle) (Math/cos angle)))
                        0 30 (fill (new-colour 255 (int (+ 50 angle)) 0)))))

  (draw-pattern)

  ; Pattern 1
  (def example-2 (create-image "Example" 1000 1000))
  (render-all example-2
              (clear green)
              (circle 500 500 0 300 (fill pink))
              (circle 500 500 1000 200 (fill (darker green)))
              (flower 500 500 2000 300 0 pink)
              (flower 500 500 3000 12 0 (new-colour 255 150 0))
              (flower 500 500 4000 120 0 (new-colour 255 150 0))
              (flower 500 500 5000 320 0 (new-colour 255 150 0))
              (flower 500 500 6000 320 0 (new-colour 0 0 255))
              (polygon [450 550 550 450]
                       [450 450 550 550]
                       7000 (fill (new-colour 100 0 255))))

  (def yoolik1 (create-image "yoolik1" 1000 1000))
  (do (render-all yoolik1
                  (clear (new-colour 0 0 255))
                  (circle 1000 1000 0 353 (fill Color/ORANGE))
                  (flower 1000 1000 0 500 0 pink)
                  (polygon [950 1050 1100 1050 950 900]
                           [900 900 1000 1100 1100 1000]
                           1
                           (fill (new-colour 150 150 255))
                           )
                  (circle 500 500 0 250 (merge (stroke (new-colour 200 0 200) 20)
                                               (fill Color/ORANGE)))
                  (circle 500 500 0 176 (fill (new-colour 0 0 255)))
                  (flower 500 500 0 250 0 pink)
                  (circle 500 500 0 100 (fill (new-colour 170 0 180)))

                  ;(circle 500 500 0 100 (fill (new-colour 255 255 0)))

                  (regular-polygon 500 500 1 3 50 0 (fill (new-colour 255 255 0)) false)
                  (regular-polygon 500 500 1 3 50 (/ TAU 2) (fill (new-colour 255 255 0)) false)

                  (let [x-dist 50
                        y-dist 50
                        star-size 40]
                    [(regular-polygon x-dist y-dist 1 5 star-size 0.12 (fill (new-colour 255 0 0)) true)
                     (regular-polygon x-dist (- 1000 y-dist) 1 5 star-size (- (/ TAU 2) 0.12) (fill (new-colour 255 0 0)) true)
                     (regular-polygon (- 1000 x-dist) y-dist 1 5 star-size -0.12 (fill (new-colour 255 0 0)) true)
                     (regular-polygon (- 1000 x-dist) (- 1000 y-dist) 1 5 star-size (+ (/ TAU 2) 0.12) (fill (new-colour 255 0 0)) true)])
                  )
      (spit-image-to-file yoolik1 "yoolik-1"))

  )