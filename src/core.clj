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
  {:path-driver      "/usr/lib/chromium-browser/chromedriver"
   :path-browser     "/usr/bin/chromium-browser"
   :path-screenshots "screenshots/"
   :wait             1})

(def chromium-opts
  {:path-driver  (:path-driver config)
   :path-browser (:path-browser config)
   ;; :load-strategy :normal
   :headless     true
   :dev
   {:perf
    {:level      :all
     :network?   true
     :page?      true
     :interval   1000
     :categories [:devtools]}}})

(def top250-data
  (map #(select-keys % [:Id :Hostname :URL :Démarche])
       (csv/slurp-csv "data/a23f3995-fcfb-414c-ae3d-82adb90c07cc")))

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

(defn website-infos [url]
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
          (e/screenshot c (str (:path-screenshots config) i))
          (reset! s (e/get-source c))
          (reset! l (edev/get-performance-logs c)))
        (println "Done gathering metadata for" url)
        (merge
         {:url              url
          :capture-filename i
          :using-ga?        (re-find #"UA-[0-9]+-[0-9]+" @s)}
         (website-html-infos @s)
         (website-logs-infos @l)))
      (catch Exception e
        (println
         (str "Can't fetch data for " url ": "
              (:cause (Throwable->map e))))))))

(defn -main [& [arg]]
  ;; (website-infos "https://bzg.fr")
  (println arg)
  )

;; (-main)

