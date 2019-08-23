(ns cljs-debugger.browser
  (:require [cljs.pprint :as pp]))

(enable-console-print!)

(println "running")

;; (defn on-click [_evt]
;;   (println "on click")
;;   (let [n (.-length (js/document.querySelectorAll "*"))]
;;     (js/console.log (str "there are " n " elments in the DOM"))))





(def h 500)
(def w 500)

(defn clear []
  (let [canvas (js/document.querySelector "canvas")]
    (set! (.-width canvas) h)
    (set! (.-height canvas) w)))

(clear)


(defn random-color []
  (letfn [(rand-n [] (js/Math.round (* 255 (js/Math.random))))]
    (pp/cl-format nil "rgb(~d,~d,~d)" (rand-n) (rand-n) (rand-n))))


(defn render-stuff []
  (let [canvas (js/document.querySelector "canvas")
        ctx (-> canvas (.getContext "2d"))
        n 3
        cell-height (/ h n)]
    (doseq [row (range n)
            col (range n)]
      (js-debugger)
      (.moveTo ctx (* col cell-height) (* row cell-height))
      (set! (.-fillStyle ctx) (random-color))
      (.fillRect ctx
                 (* col cell-height) (* row cell-height)
                 cell-height cell-height))))


(.addEventListener
 (js/document.querySelector "input[type=button]")
 "click" #(render-stuff))
