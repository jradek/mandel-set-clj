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


(with-open [out (output-stream (file "output/red.ppm"))]
  (do
    (.write out (.getBytes (ppm-header 20 20)))
    (doseq [_ (range 400)]
      (.write out (color-value 0 0 255)))))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
