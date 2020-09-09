(ns url-spider.core
  (:gen-class)
  (:require [clojure.core.async
             :as a
             :refer [>! >!! <! <!! go go-loop chan alt!]])
  (:import [org.jsoup Jsoup]))

(comment
  (def -soup (-> "test_doc.html"
                 (slurp)
                 (Jsoup/parse))))

(defn print-chan
  "Create a channel that will forever print its input"
  [& args]
  (let [c (apply chan args)]
    (go (loop []
          (println "[IN]" (<! c))
          (recur)))
    c))

;; This is rather straightforward. However... it requires a double map to get it
;; working on a collection of elements it seems
(defn el->href
  [el]
  (-> el (.attr "href")))

(defn mapstuff [coll] (mapcat #(map el->href %) coll))

(def print> (print-chan 1 (mapcat #(map el->href %))))
(def el-href> (chan 1 (mapcat #(map el->href %))))
;; (def links (print-chan 1 (mapcat identity)))


(defn process-hrefs
  [input>]
  (go (loop [visited #{}]
        (let [next-url (<! input>)]
          (println "GO ->" next-url)
          (Thread/sleep 300) ;; TMP While debugging let's throttle this thing a bit
          (when-not (contains? visited next-url)
            (println "Should fetch " next-url)
            (-> next-url
                (slurp)
                (Jsoup/parse)
                (.select "a")
                (>! input>)))
          (recur (conj visited next-url))))))

(process-hrefs el-href>)

;; (java.net.URL. "/")

(comment
  (-> -soup (.select "a") (first)
      (el->href))

  (-> -soup (.select "a")
      (->> (map el->href)))

  (def -soup (-> "https://iansinnott.com" (slurp) (Jsoup/parse) (.select "a")))
  (let [els (-> -soup (.select "a"))]
    (go (>! el-href> els))))

(let [els (-> "https://iansinnott.com" (slurp) (Jsoup/parse) (.select "a"))]
  (go (>! print> els)))

(eduction (map inc) [1 7 2])
(def c> (chan))
(a/put! c> (rand))
(println (<!! c>))

(go (>! c> (rand)))

(defn drain-chan [c>]
  (go (loop []
        (let [x (<! c>)]
          (if-not (nil? x)
            (do (println "[drained]" x)
                (recur))
            (println "DONE"))))))

(drain-chan c>)
(drain-chan (a/to-chan! ["hey", "you" "there"]))

(drain-chan (a/merge [(go "sup")
                      (go "Wee")]))

(def p (promise))
(deliver p "SUP")
p

(comment
  (when-not (nil? "a")
    (println "wee"))
  (when-some [a nil] ;; When some is a shorthand for (when (not (nil? ...
    (println "SUP" a))
  (when-let [a "he"] ;; When let is ALSO... ? a shorthand for (when (not (nil? ...
    (println "SUP" a)))

(comment
  (def c (chan 30 (mapcat #(map el->href %))))
  (>!! c (-> -soup (.select "a")))
  (let [els (-> "https://lab.iansinnott.com" (slurp) (Jsoup/parse) (.select "a"))]
    (>!! c els))
  (go (println "GOT" (<! c))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [base-url (first args)]
    (when (empty? base-url) (throw (Exception. "No URL provided")))))

(comment
  (-main "https://iansinnott.com/"))
