(ns mandel.core
  (:require [clojure.java.io :refer [output-stream file]])
  (:gen-class))

(defn ppm-header
  "Write the Header for a PPM File"
  [x-dim y-dim]
  (clojure.string/join ["P6\n" x-dim " " y-dim " 255\n"]))

(defn clamp-255
  [x]
  (max (min x 255) 0))

(defn to-byte
  [x]
  (let [val (clamp-255 x)]
    (if (> val 127)
      (byte (- val 256))
      (byte val))))


(defn color-value [r g b]
  (byte-array  [(to-byte r) (to-byte g) (to-byte b)]))


(with-open [out (output-stream (file "output/red.ppm"))]
  (do
    (.write out (.getBytes (ppm-header 20 20)))
    (doseq [_ (range 400)]
      (.write out (color-value 0 0 255)))))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
