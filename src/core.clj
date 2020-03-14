(ns core
  (:require [clojure.java.io :as io]
            [etaoin.api :as e]
            [clojure.string]
            [semantic-csv.core :as csv]
            [clj-http.lite.client :as http]
            [etaoin.dev :as edev]
            [hickory.core :as h])
  (:gen-class))

(def config
  {:path-driver        "/usr/lib/chromium-browser/chromedriver"
   :path-browser       "/usr/bin/chromium-browser"
   :path-screenshots   "screenshots/"
   :data-path          "data/"
   :top250-output-file "top250.csv"
   :gouvfr-output-file "gouvfr.csv"
   :wait               0.1})

(def chromium-opts
  {:path-driver  (:path-driver config)
   :path-browser (:path-browser config)
   ;; :load-strategy :normal ;; FIXME: what does it change?
   :headless     true
   :dev
   {:perf
    {:level      :all
     :network?   true
     :page?      true
     :interval   1000
     :categories [:devtools]}}})

(def top250-data (agent nil))
(def gouvfr-data (agent nil))

(def top250-init
  (distinct
   (map #(select-keys % [:Id :Démarche :URL])
        (csv/slurp-csv (str (:data-path config)
                            "a23f3995-fcfb-414c-ae3d-82adb90c07cc")))))

(def gouvfr-init nil)

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
      (let [c (e/chrome chromium-opts)]
        (println "Gathering metadata for" url)
        (e/with-wait (:wait config)
          (e/go c url)
          (when-not top250?
            (e/screenshot
             c (str (:data-path config)
                    (:path-screenshots config) i)))
          (reset! s (e/get-source c))
          (reset! l (edev/get-performance-logs c)))
        (println "Done gathering metadata for" url)
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

(defn generate-data [input output]
  (let [top250? (= input top250-init)]
    (doseq [e input]
      (send output conj
            (merge e (website-infos (:URL e) top250?))))
    (csv/spit-csv
     (str (:data-path config)
          (if top250?
            (:top250-output-file config)
            (:gouvfr-output-file config)))
     (deref output))))

(defn -main []
  (generate-data top250-init top250-data)
  (generate-data gouvfr-init gouvfr-data))

;; (time (-main))

