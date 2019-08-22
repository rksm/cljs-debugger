(ns cljs-debugger-example.core
  (:require [cljs.pprint :as pp]))


(enable-console-print!)

(println "running!!!")

(defn random-color []
  (letfn [(rand-n [] (js/Math.round (* 255 (js/Math.random))))]
    (pp/cl-format nil "rgb(~d,~d,~d)" (rand-n) (rand-n) (rand-n))))


(defn render-stuff []
  (let [canvas (js/document.querySelector "canvas")
        h 500
        w 500
        ctx (-> canvas (.getContext "2d"))

        ;; (-> ctx .save)
        ;; (-> ctx .restore)
        ;; (-> ctx (.translate (/ w 2) (/ h 2)))

        n 3
        cell-height (/ h n)]
    (set! (.-width canvas) h)
    (set! (.-height canvas) w)
    (doseq [row (range n)
            col (range n)]
      (js-debugger)
      (.moveTo ctx (* col cell-height) (* row cell-height))
      (set! (.-fillStyle ctx) (random-color))
      (.fillRect ctx
                 (* col cell-height) (* row cell-height)
                 (* (inc col) cell-height) (* (inc row) cell-height)))))



(render-stuff)
