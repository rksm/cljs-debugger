(ns cljs-debugger.browser
  ;; (:require [clojure.browser.repl :as repl]
)

(enable-console-print!)

(println "running")

(defn on-click [_evt]
  (let [msg "you"]
    (println "on click")
    (js-debugger)
    (js/console.log (str msg " clicked"))))


(defonce on-click-registered
  (do (.addEventListener js/document "click" on-click) on-click))


;; (.removeEventListener js/document "click" on-click)

;; (-> js/document (.removeEventListener "click" on-click))

;; (repl/connect "http://localhost:9000/repl")

