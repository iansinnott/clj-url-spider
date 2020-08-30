(ns url-spider.core
  (:gen-class)
  (:import [org.jsoup Jsoup]))


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
