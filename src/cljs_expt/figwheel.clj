(ns cljs-debugger.figwheel
  (:require [figwheel.main :refer [build-registry]]))


(def repl-opts (-> @build-registry (get "dev") :repl-options))

