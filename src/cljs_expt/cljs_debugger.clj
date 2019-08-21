(ns cljs-debugger.cljs-debugger
  (:require [cljs-debugger.devtools :as devtools]))

(defonce debugging-state (atom {:chrome-connection nil
                                :chrome-events nil
                                :nrepl-debug-init-msg nil}))



(defn disconnect! []
  (swap! debugging-state merge (devtools/disconnect @debugging-state)))


(defn connect!
  ([]
   (connect! nil))
  ([opts]
   (disconnect!)
   (swap! debugging-state merge (devtools/connect opts))
   (println "cljs debugger is connected!")))

(defn nrepl-debug-init!
  [debug-init-msg]
  (swap! debugging-state assoc :nrepl-debug-init-msg debug-init-msg))





(comment

  (disconnect!)
  (connect!)

  (require '[clj-chrome-devtools.commands.debugger :as chrome-dbg])

  (def c (-> @debugging-state :chrome-connection))
  (chrome-dbg/resume c {})

  )
