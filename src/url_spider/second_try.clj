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

(defn pingpong []
  (let [c (chan)
        ping (fn [] (>! c "ping"))
        pong (fn [] (>! c "pong"))
        invert #(if (= % "ping") "pong" "ping")]
    (go (loop [prev "pong"]
          (let [next (<! c)]
            (when (= prev next)
              (println "You must " (invert next) "!"))
            (recur next))))
    [ping pong c]))

(comment
  (def _ (pingpong))
  (def ch (last _))
  (>!! ch "ping")
  (>!! ch "pong"))

(let [[ping pong c] (pingpong)]
  (>!! c "ping")
  (>!! c "ping"))
  ;; (pong)
  ;; (ping)
  ;; (ping) ;; Expect "You must pong!"
  ;; (pong)
  ;; (pong)) ;; Expect "You must ping!"

;; Create a password machine
(def p-machine (password-machine))

;; Run these in order to unlock, run out of order or simply run the first one
;; twice to cause the machine to ask for a retry
(>!! p-machine 1)
(>!! p-machine 2)
(>!! p-machine 3)

(defn el->attrs
  "Extract the attributes of an Element into a plain map"
  [^org.jsoup.nodes.Element el]
  (let [attrs (-> el (.attributes))]
    (reduce (fn [coll x]
              (assoc coll (keyword (.getKey x)) (.getValue x))) {} attrs)))

(defn el->data
  "Create plain data structures from a Jsoup element"
  [^org.jsoup.nodes.Node el]
  (cond
    (instance? org.jsoup.nodes.TextNode el) (.text el)
    (instance? org.jsoup.nodes.Comment el) ""
    :else (let [tag-name (.tagName el)
                attrs (el->attrs el)
                children (->> (.childNodes el)
                              (map el->data) ;; Recurse
                              (filter #(not= % "")))] ;; Filter out empty strings
            [tag-name attrs children])))

(defn select
  [selector soup]
  (-> soup (.select selector) (->> (map el->data))))

(comment
  (def -soup
    (-> "https://iansinnott.com/"
        (slurp)
        (Jsoup/parse)))
  (-> -soup
      (.select "a")
      (first)
      (.childNodes)
      (nth 2)
      (el->data))
  (->> -soup (select "a") (map (comp :href #(second %)))))

(defn links-all-pages
  [x]
  (let [soup (Jsoup/parse (slurp x))
        links (->> soup
                   (select "a")
                   (map (fn [[_ attrs]] (:href attrs)))

                   ;; Try to normalize URLs a bit
                   (map (fn [href] (if (.startsWith href "/") (str x href) href)))
                   (map (fn [href] (if (not (.contains href "://")) (str x "/" href) href)))
                   (vec))]
    links))

(comment
  (links-all-pages "https://lab.iansinnott.com"))

(defn queue [x]
  (conj clojure.lang.PersistentQueue/EMPTY x))

(defn subdomain?
  "Is domain a a subdomain of domain b?"
  [a b]
  (let [ua (-> a (java.net.URL.) (.getHost))
        ub (-> b (java.net.URL.) (.getHost))]
    (.contains ua ub)))

(defn same-origin?
  "Do two URLs have the same host?"
  [a b]
  (let [ua (-> a (java.net.URL.) (.getHost))
        ub (-> b (java.net.URL.) (.getHost))]
    (= ua ub)))

(comment
  (subdomain?  "https://iansinnott.com/"  "https://iansinnott.com/")
  (same-origin?  "https://iansinnott.com/"  "https://iansinnott.com/")
  (subdomain?   "https://blog.iansinnott.com/" "https://iansinnott.com/")
  (same-origin?   "https://blog.iansinnott.com/" "https://iansinnott.com/")
  (subdomain?  "https://instagram.com/"  "https://blog.iansinnott.com/")
  (same-origin?  "https://instagram.com/"  "https://blog.iansinnott.com/"))

(->> ["hey" "ey"]
     (sort-by count))
(def include-subdomains false)
(def root-url "https://iansinnott.com/")

(comment
  (links-all-pages "https://iansinnott.com/")
  (->> -soup (select "a") (map (fn [[_ attrs]] (:href attrs)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [root-url (first args)]
    (let [state (atom {:visited #{}
                       :skipped #{}
                       :failed #{}
                       :queue (queue root-url)})]
      (while (not (empty? (:queue @state)))
        (let [x (peek (:queue @state))
              visited? (contains? (:visited @state) x)
              should-skip? (and
                            (not (same-origin? x root-url))
                            (not (subdomain? x root-url)))]
          (try
            (do
              (println (str "Maybe... " x))
              (if should-skip?
                (swap! state update :skipped conj x)
                (when (not visited?)
                  (println (str "DO IT!" x))
                  (Thread/sleep 100)
                  (swap! state update :visited conj x)
                  (let [links (links-all-pages x)]
                    (swap! state update :queue (fn [y]
                                                 (apply conj y links))))))
              (swap! state update :queue pop))
            (catch Exception e
              (println "Failed" x)
              (swap! state update :failed conj x)))))
      @state)))

(comment
  (-main "https://iansinnott.com/"))
