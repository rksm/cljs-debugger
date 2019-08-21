(ns cljs-debugger.repl
  (:require
   [cljs-debugger.build :refer [opts cenv]]
   [cljs.repl :as repl]
   [cljs.repl.browser :as browser]
   clojure.core.async
   [clojure.core.async :as a]
   [clojure.java.io :as io]))


(defn make-async-reader []
  (let [read-chan (a/chan 1)
        r (proxy [java.io.Reader] []
            (read [cbuf off len]
              (println "reading" off len)
              (if-let [string (a/<!! read-chan)]
                (do (doseq [[i c] (map-indexed vector string)]
                      (aset cbuf (+ off i) c))
                    (count string))
                -1))
            (close []
              (println "closed")
              (a/close! read-chan)))]
    {:reader (clojure.lang.LineNumberingPushbackReader. r)
     :chan read-chan}))



(def input (make-async-reader))

(def repl-opts {:analyze-path "src"
                :repl-verbose true
                :launch-browser false
                :src "src"
                :static-dir "resources/public"
                :port 9000
                ;; :need-prompt (constantly false)
                ;; :read repl-read
                ;; :reader (java.io.PushbackReader/nullReader)
                ;; :reader (:reader input)
                })


(def env (apply browser/repl-env (apply concat repl-opts)))




(comment


  (a/put! (:chan input) "(enable-console-print!)\n")
  (a/put! (:chan input) "(println \"test\")\n")
  (a/close! (:chan input))

  (future (binding [*in* (:reader input)] (repl/repl env) (println "DONE!")))


  (-> env :server-state deref :socket (.close))
  (-> env :server-state deref :socket .isClosed)
  (-> env :server-state deref)


  (binding [cljs.repl/*repl-env* env
            cljs.env/*compiler* cenv
            cljs.analyzer/*cljs-ns* 'cljs.user]
    (repl/eval-cljs env @cenv '(+ 1 2)))



  (do (require 'cljs.repl.browser) (require 'cljs.repl)
      (cljs.repl/repl (cljs.repl.browser/repl-env))))

;; (def input-pipe (make-input-output-pipe))

;; (def repl-opts {:analyze-path "src"
;;                 :repl-verbose true
;;                 :launch-browser false
;;                 :src "src"
;;                 :static-dir "resources/public"
;;                 :port 9000
;;                 ;; :need-prompt (constantly false)
;;                 :read repl-read
;;                 ;; :reader (java.io.PushbackReader/nullReader)
;;                 :reader (:reader input-pipe)
;;                 })


;; (def env (apply browser/repl-env (apply concat repl-opts)))


(comment

  (defn make-input-output-pipe []
    (let [out (java.io.PipedOutputStream.)
          in (java.io.PipedInputStream. out)
          writer (java.io.StringWriter. out true)
          reader (clojure.lang.LineNumberingPushbackReader. (java.io.InputStreamReader. in))
          w (io/writer writer)
          close! #(do (.close out) (.close in))
          write! #(.write w %)]
      {:close! close! :write! write! :reader reader :writer writer :in in :out out}))

  (defrecord TestReader []
    java.io.Reader
    (read [^chars cbuf ^int off ^int len]
      (for [[i c] (map-indexed vector "test")]
        (aset cbuf (+ off i) c))
      (count "test"))

    (close [] (println "closed")))



  (let [content (atom "test")
        r (proxy [java.io.Reader] []
            (read [cbuf off len]
              (println "reading" off len)
              (if-let [string @content]
                (do (reset! content nil)
                    (doseq [[i c] (map-indexed vector string)]
                      (aset cbuf (+ off i) c))
                    (count string))
                -1))
            (close [] (println "closed")))]
    (with-open [s (clojure.lang.LineNumberingPushbackReader. r)]
      (binding [*in* s] (read))))





  (def sw (java.io.StringWriter.))

  (.write sw "foo")


  (java.io.StringReader. (.getBuffer sw))
  (java.io. (.getBuffer sw))


  (def f (future (with-in-str "(enable-console-print!) (println \"test\") :cljs/quit" (repl/repl env)) (println "DONE!")))
  (def f (future (with-in-str "(enable-console-print!) (println \"test\")" (repl/repl env)) (println "DONE!")))
  (def f (future (repl/repl env) (println "DONE!")))


  (future (binding [*in* (:reader input-pipe)] (repl/repl env) (println "DONE!")))


  ((:write! input-pipe) "(enable-console-print!) (println \"test\")\n")
  ((:write! input-pipe) "(println \"test\")\n")
  ((:write! input-pipe) ":cljs/quit")

  (:out input-pipe)
  (.write (:out input-pipe) java.io.StringWriter )
  (.flush (:out input-pipe))
  (.available (:in input-pipe))

  (.connect (:in input-pipe) (:out input-pipe))

  (with-in-str)

  (future (println "fffoooo?") (try
                                 (binding [*in* (:reader input-pipe)] (println "got:" (read)))
                                 (catch Exception e (println "error: " e)))
          (println "doneeee"))

  (binding [*in* (:reader input-pipe)] (read-line))
  (binding [*in* (:reader input-pipe)] (read))

  (read (java.io.BufferedInputStream. (:in input-pipe)))
  

  (.write (:writer input-pipe) "(js/console.log \"foooooo\")")
  (.flush (:writer input-pipe))
  (.ready (:reader input-pipe))




  (binding [cljs.repl/*repl-env* env
            cljs.analyzer/*cljs-ns* 'cljs.user]
    ;;(repl/eval-cljs env cenv '(+ 1 2))
    )


  (a/>!! read-chan 123)

  (do
    (a/go (repl-read 1 2))
    (a/put! read-chan "foo"))

  2
  (a/go
    (repl/repl env)
    (println "REPL DONE!!!!!!!!!!!!!!!!"))


  (#(clojure.tools.reader.reader-types/source-logging-push-back-reader
     *in*
     1 "<NO_SOURCE_FILE>"))

  (read (clojure.java.io/reader "foo"))

  (read *in*)



  env

  (defn foo [& {:keys [foo]}]
    foo)

  (apply foo [:foo 23])
  )
