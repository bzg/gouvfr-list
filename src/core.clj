(ns core
  (:require [clojure.java.io :as io]
            [etaoin.api :as e]
            [clojure.string]
            [utils :as u]
            [etaoin.dev :as edev]
            [hickory.core :as h]
            [semantic-csv.core :as csv]
            [clj-http.client :as http]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup debugging and logging

(def testing true)

(timbre/set-config!
 {:level     :debug
  :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})
  :appenders
  {:println (timbre/println-appender {:stream :auto})
   :spit    (appenders/spit-appender {:fname (:log-file u/config)})}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core function to gather data

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
        (timbre/info (str "Gathering metadata for " url "..."))
        (e/with-wait (:wait u/config)
          (e/go c url)
          (when-not top250?
            (e/screenshot
             c (str (u/path :screenshots-rel-path) i)))
          (reset! s (e/get-source c))
          (reset! l (edev/get-performance-logs c)))
        (timbre/info "... done")
        (merge
         {:using-ga? (re-find #"UA-[0-9]+-[0-9]+" @s)}
         (merge (when-not top250? {:capture-filename i})
                (website-html-infos @s))
         (website-logs-infos @l)))
      (catch Exception e
        (timbre/info
         (str "Can't fetch data for " url ": "
              (:cause (Throwable->map e))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upload to data.gouv.fr

(def datagouv-api "https://www.data.gouv.fr/api/1")

(def datagouv-api-token (System/getenv "DATAGOUV_API_TOKEN"))

(def csv-file-path (u/path :gouvfr-output-file))

;; https://www.data.gouv.fr/fr/datasets/liste-de-sites-en-gouv-fr/
(def dataset "5e6cda15634f41398715ea45")
(def resource "a0f7b15f-2a53-44cb-8182-2a440b78abe8")

(def datagouv-endpoint-format
  (str datagouv-api "/datasets/" dataset
       "/resources/%s/upload/"))

(def datagouv-api-headers
  {:headers {"Accept"    "application/json"
             "X-Api-Key" datagouv-api-token}})

(defn upload-gouvfr-csv []
  (timbre/info "Upload gouvfr.csv to data.gouv.fr...")
  (if-not (.exists (io/file csv-file-path))
    (timbre/error "Upload aborted: gouvfr.csv does not exist")
    (if-let [res (try (http/post
                       (format datagouv-endpoint-format resource)
                       (merge datagouv-api-headers
                              {:insecure?     true
                               :cookie-policy :standard}
                              {:multipart [{:name    "file"
                                            :content (io/file csv-file-path)}]}))
                      (catch Exception _ nil))]
      (timbre/info "... done.")
      (timbre/info "... NOT done!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(def top250-data (atom nil))
(def gouvfr-data (atom nil))

(defn generate-data [input output & [top250?]]
  (doseq [e (if testing (take 5 input) input)]
    (swap! output conj
           (merge e (website-infos (:base-url e) top250?))))
  (csv/spit-csv
   (u/path (if top250? :top250-output-file :gouvfr-output-file))
   (deref output)))

(defn generate-data-top250 [init?]
  (when (or init? (not (.exists (io/as-file (u/path :top250-init-file)))))
    (u/top250-init))
  (let [top250 (csv/slurp-csv (u/path :top250-init-file))]
    (generate-data top250 top250-data :top250)))

(defn generate-data-gouvfr [init?]
  (when (or init? (not (.exists (io/as-file (u/path :gouvfr-init-file)))))
    (u/top250-init))
  (let [top250 (csv/slurp-csv (u/path :gouvfr-init-file))]
    (generate-data top250 top250-data)))

(defn -main [& [type init?]]
  (condp = type
    "top250" (generate-data-top250 init?)
    "gouvfr" (generate-data-gouvfr init?)
    (timbre/error "First arg must by \"top250\" or \"gouvfr\""))
  ;; TODO:
  ;; (when (and (not testing)
  ;;            (.exists (io/as-file (u/path :top250-output-file))))
  ;;   (upload-top250-csv))
  (when (and (not testing)
             (.exists (io/as-file (u/path :gouvfr-output-file))))
    (upload-gouvfr-csv)))
