(ns core
  (:require [clojure.java.io :as io]
            [etaoin.api :as e]
            [clojure.string]
            [utils :as u]
            [semantic-csv.core :as csv]
            [etaoin.dev :as edev]
            [hickory.core :as h])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization

(def testing true)
(def top250-data (atom nil))
(def gouvfr-data (atom nil))

(if-not
    (or testing
        (.exists (io/as-file
                  (u/path :top250-init-file))))
  (utils/top250-init))

(if-not
    (or testing
        (.exists (io/as-file
                  (u/path :gouvfr-init-file))))
  (utils/gouvfr-init))

(def top250-init
  (csv/slurp-csv (u/path :top250-init-file)))

(def gouvfr-init
  (csv/slurp-csv (u/path :gouvfr-init-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gather data

(defn total-content-length [reqs]
  (reduce
   +
   (map (fn [ct]
          (read-string (or (:Content-Length ct)
                           (:content-length ct) "0")))
        (remove
         (fn [req] (re-find
                    #"video/"
                    (or (:Content-Type req)
                        (:content-type req) "")))
         (remove nil? (map #(get-in % [:response :headers]) reqs))))))

(defn website-html-infos [s]
  (let [h             (filter #(not (string? %)) (h/as-hiccup (h/parse s)))
        s             (tree-seq sequential? #(filter vector? %) h)
        count-tags    (count (rest s))
        get-vals      (fn [k] (filter #(= (first %) k) s))
        get-meta-vals (fn [ks prop]
                        (:content
                         (last (first (filter #(= ks (prop (last %)))
                                              (get-vals :meta))))))
        title         (last (first (get-vals :title)))
        description   (get-meta-vals "description" :name)
        keywords      (get-meta-vals "keywords" :name)]
    {:title       (or title "")      
     :tags        count-tags
     :description (or description "")
     :keywords    (or keywords "")
     :og:image    (or (get-meta-vals "og:image" :property) "")}))

(defn website-logs-infos [logs]
  (let [requests (edev/logs->requests logs)
        logs0    (filter #(= (:method %) "Network.responseReceived")
                         (map :message logs))
        logs1    (first (filter ;; FIXME: relying on the first response?
                         #(get-in % [:params :response :url])
                         logs0))]
    {:requests-number (count requests)
     :is-secure?      (= (get-in logs1 [:params :response :securityState]) "secure")
     :content-length  (total-content-length requests)}))

(defn website-infos [url top250?]
  (let [s (atom nil)
        l (atom nil)
        i (str (clojure.string/replace
                (last (re-find #"(?i)(https?://)(.+[^/])" url))
                #"/" "-") ".jpg")]
    (try
      (let [c (e/chrome (:chromium-opts u/config))]
        (println "Gathering metadata for" url "...")
        (e/with-wait (:wait u/config)
          (e/go c url)
          (when-not top250?
            (e/screenshot
             c (str (u/path :screenshots) i)))
          (reset! s (e/get-source c))
          (reset! l (edev/get-performance-logs c)))
        (println "... done")
        (merge
         {:using-ga? (re-find #"UA-[0-9]+-[0-9]+" @s)}
         (when-not top250?
           (merge {:capture-filename i}
                  (website-html-infos @s)))
         (website-logs-infos @l)))
      (catch Exception e
        (println
         (str "Can't fetch data for " url ": "
              (:cause (Throwable->map e))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(defn generate-data [in out & [limit]]
  (let [top250? (= in top250-init)
        input   (if limit (take limit in) in)]
    (doseq [e input]
      (swap! out conj (merge e (website-infos (:URL e) top250?))))
    (csv/spit-csv
     (u/path (if top250? :top250-output-file :gouvfr-output-file))
     (deref out))))

(defn -main []
  (generate-data top250-init top250-data (when testing 3))
  (generate-data gouvfr-init gouvfr-data (when testing 3)))

;; (time (-main))

