(ns cljs-debugger.browser
  ;; (:require [clojure.browser.repl :as repl]
)

(enable-console-print!)

(println "running")

(defn on-click [_evt]
  (println "on click")
  (js-debugger)
  (let [n (.-length (js/document.querySelectorAll "*"))]
    (js/console.log (str "there are " n " elments in the DOM"))))

(defonce on-click-registered
  (do (.addEventListener js/document "click" on-click) on-click))


;; (.removeEventListener js/document "click" on-click)

;; (-> js/document (.removeEventListener "click" on-click))

;; (repl/connect "http://localhost:9000/repl")

