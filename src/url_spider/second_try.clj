(ns url-spider.second-try
  (:require [clojure.core.async
             :as a
             :refer [>! >!! <! <!! go go-loop chan alt!]]))

(defn blah []
  (println "ok, fine"))

;; Printer will create an immediate consumer. My own wording. A challen that has
;; a pre-existing listener which will consume any events passed to it
(defn printer []
  (let [c (chan)]
    (go-loop []
      (println "<--" (<! c))
      (recur))
    c))

(def my-chan (printer))

(do
  (println "-->" "hey")
  (>!! my-chan "hey"))

(defn password-machine
  "Create a password machine. Say what now? It's an async passowrd checker. So
  you can enter the number over time. As soon as you're wrong it will tell you
  to retry. This is just great for guess and check so it would not be very
  secure in real life, but it's interesting to see channels at work"
  []
  (let [c (chan)]
    (go (loop []
          (if (and (= (<! c) 1)
                   (= (<! c) 2)
                   (= (<! c) 3))
            (do (println "UNLOCKED!")
                (recur))
            (do (println "Retry!")
                (recur)))))
    c))

;; Create a password machine
(def p-machine (password-machine))

;; Run these in order to unlock, run out of order or simply run the first one
;; twice to cause the machine to ask for a retry
(>!! p-machine 1)
(>!! p-machine 2)
(>!! p-machine 3)
