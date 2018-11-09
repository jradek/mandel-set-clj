(ns mandel.core
  (:require [clojure.java.io :refer [output-stream file]])
  (:gen-class))

(defn ppm-header
  "Write the Header for a PPM File"
  [w h]
  (clojure.string/join ["P6\n" w " " h " 255\n"]))


(defn clamp
  "Constrain x to range [min-value ... max-value]"
  [x & {:keys [min-value max-value]}]
  (let [min-value (or min-value 0)
        max-value (or max-value 100)]
    (max (min x max-value) min-value)))


(defn to-byte
  [x]
  (let [val (clamp x :max-value 255)]
    (if (> val 127)
      (byte (- val 256))
      (byte val))))


(defn color-value [r g b]
  (byte-array  [(to-byte r) (to-byte g) (to-byte b)]))

(defn sqr-complex
  [[x y]]
  [(- (* x x) (* y y))
   (* 2 x y)])

(defn mag-complex
  [[x y]]
  (+ (* x x) (* y y)))

(defn add-complex
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn mandel-func
  [[x0 y0] max-iterations]
  (loop [iter 0
         z (sqr-complex [x0 y0])]
    (if (or (> iter max-iterations) (> (mag-complex z) 4))
      iter
      (recur
        (inc iter)
        (add-complex z [x0 y0])))))

(defn iter-to-color
  [x max-iterations]
  (cond
    (> x max-iterations) (color-value 255 255 255)
    (<= x 63) (let [tmp (- 255 (* 4 x))]
                  (color-value 255 tmp tmp))
    :else (color-value 255 (- x 63) 0)))

(defn iter-to-rgb
  [x max-iterations]
  (cond
    (> x max-iterations) [255 255 255]
    (<= x 63) (let [tmp (- 255 (* 4 x))]
                [255 tmp tmp])
    :else [255 (- x 63) 0]))

(defn step
  [x0 x1 n]
  (/ (- x1 x0) n))


(defn dimensions
  [steps]
  (let [m {:x-min -2.0 :y-min -1.5 :x-max 1.0 :y-max 1.5 :steps steps}]
    (merge m {:x-step (step (:x-min m) (:x-max m) steps)
              :y-step (step (:y-min m) (:y-max m) steps)})))


(defn board
  [steps max-iterations]
  (let [dim (dimensions steps)]
    (for [y (range (:y-max dim) (:y-min dim) (- 0 (:y-step dim)))
          x (range (:x-min dim) (:x-max dim) (:x-step dim))]
      (mandel-func [x y] max-iterations))))


(with-open
  [out (output-stream (file "output/red.ppm"))]
  (let [dim 500
        max-iter 200]
    (do
      (.write out (.getBytes (ppm-header dim dim)))
      (doseq [x (map #(iter-to-color %1 max-iter)
                     (board dim max-iter))]
        (.write out x)))))

(with-open
  [out (output-stream (file "output/mono.ppm"))]
  (let [dim 16
        max-iter 255]
    (do
      (.write out (.getBytes (ppm-header dim dim)))
      (doseq [x (map #(iter-to-rgb %1 max-iter) (range 256))]
        (.write out (color-value (get x 0) (get x 1) (get x 2)))))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
