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

(def testing false)

(timbre/set-config!
 {:level     :debug
  :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})
  :appenders
  {:println (timbre/println-appender {:stream :auto})
   :spit    (appenders/spit-appender {:fname (:log-file u/config)})}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core function to gather data

;; https://stackoverflow.com/questions/6694530/executing-a-function-with-a-timeout
(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut))))

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

(defn site-html-infos [s]
  (let [h             (filter #(not (string? %)) (h/as-hiccup (h/parse s)))
        s             (tree-seq sequential? #(filter vector? %) h)
        count-tags    (count (rest s))
        get-vals      (fn [k] (filter #(= (first %) k) s))
        get-meta-vals (fn [ks prop]
                        (:content
                         (last (first (filter #(= ks (prop (last %)))
                                              (get-vals :meta))))))
        title         (last (first (get-vals :title)))
        viewport      (get-meta-vals "viewport" :name)
        description   (get-meta-vals "description" :name)
        keywords      (get-meta-vals "keywords" :name)]
    {:title       (u/string-replace-newline (or title ""))
     :viewport    (or viewport "")
     :tags        count-tags
     :description (u/string-replace-newline (or description ""))
     :keywords    (u/string-replace-newline (or keywords ""))
     :og:image    (or (get-meta-vals "og:image" :property) "")}))

(defn site-logs-infos [logs]
  (let [requests (edev/logs->requests logs)
        logs0    (filter #(= (:method %) "Network.responseReceived")
                         (map :message logs))
        logs1    (first (filter ;; FIXME: relying on the first response?
                         #(get-in % [:params :response :url])
                         logs0))]
    {:requests-number (count requests)
     :is-secure?      (= (get-in logs1 [:params :response :securityState]) "secure")
     :content-length  (total-content-length requests)}))

(defn site-infos [chrome-session url top250?]
  (let [c chrome-session
        s (atom nil)
        l (atom nil)
        i (str (u/url-encode (u/url-no-protocol url)) ".jpg")
        w (:driver-wait u/config)
        t (:driver-timeout u/config)]
    (timbre/info (str "Gathering metadata for " url "..."))
    (try
      (do (e/with-wait w (e/go c url)
            (when-not top250?
              (e/screenshot
               c (str (u/path :screenshots-rel-path) i)))
            ;; Prevent downloading illformated pages
            (timeout t #(reset! s (e/get-source c)))
            (timeout t #(reset! l (edev/get-performance-logs c))))
          (timbre/info "... done")
          (merge
           {:using-ga? (nil? (re-find #"UA-[0-9]+-[0-9]+" (or @s "")))}
           (merge (when-not top250? {:capture-filename i})
                  (site-html-infos (or @s "")))
           (site-logs-infos @l)))
      (catch Exception e
        (timbre/info
         (str "Can't fetch data for " url ": "
              (:cause (Throwable->map e))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upload to data.gouv.fr

(def datagouv-api "https://www.data.gouv.fr/api/1")
(def datagouv-api-token (System/getenv "DATAGOUV_API_TOKEN"))

;; https://www.data.gouv.fr/fr/datasets/liste-de-sites-en-gouv-fr/
(def gouvfr-file-path (u/path :gouvfr-output-file))
(def gouvfr-dataset "5e6cda15634f41398715ea45")
(def gouvfr-resource "a0f7b15f-2a53-44cb-8182-2a440b78abe8")

;; https://www.data.gouv.fr/fr/datasets/observatoire-de-la-dematerialisation-de-qualite-tableau-de-bord-des-demarches-phares-de-letat/
(def top250-file-path (u/path :top250-output-file))
(def top250-dataset "5d0a5c09634f412301dba116")
(def top250-resource "e1de8987-7d9a-4885-987f-80f2e9c63c3f")

(defn datagouv-endpoint-format [d r]
  (str datagouv-api "/datasets/" d "/resources/" r "/upload/"))

(def datagouv-api-headers
  {:headers {"Accept"    "application/json"
             "X-Api-Key" datagouv-api-token}})

(defn upload-csv [file-path dataset resource]
  (timbre/info "Upload gouvfr.csv to data.gouv.fr...")
  (if-not (.exists (io/file file-path))
    (timbre/error
     (format "Upload aborted: %s does not exist" file-path))
    (if-let [res (try (http/post
                       (datagouv-endpoint-format dataset resource)
                       (merge datagouv-api-headers
                              {:insecure?     true
                               :cookie-policy :standard}
                              {:multipart [{:name    "file"
                                            :content (io/file file-path)}]}))
                      (catch Exception _ nil))]
      (timbre/info "... done.")
      (timbre/info "... NOT done!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(def top250-data (atom nil))
(def gouvfr-data (atom nil))

(defn generate-data [input output & [top250?]]
  (let [c (e/chrome (:chromium-opts u/config))]
    (doseq [e (if testing (take 5 input) input)]
      (swap! output conj (merge e (site-infos c (:URL e) top250?)))))
  (when top250? (swap! output u/top250-csv-fields-to-fr))
  (csv/spit-csv
   (u/path (if top250? :top250-output-file :gouvfr-output-file))
   (deref output)))

(defn generate-data-top250 [init?]
  (when (or init? (not (.exists (io/as-file (u/path :top250-init-file)))))
    (u/top250-init))
  (generate-data
   (csv/slurp-csv (u/path :top250-init-file)) top250-data :top250))

(defn generate-data-gouvfr [init?]
  (when (or init? (not (.exists (io/as-file (u/path :gouvfr-init-file)))))
    (u/gouvfr-init))
  (generate-data
   (csv/slurp-csv (u/path :gouvfr-init-file)) gouvfr-data))

(defn -main [& [type init?]]
  (condp = type
    "top250" (do (generate-data-top250 init?)
                 (when-not testing
                   (upload-csv (u/path :top250-output-file)
                               top250-dataset top250-resource)))
    "gouvfr" (do (generate-data-gouvfr init?)
                 (when-not testing
                   (upload-csv (u/path :gouvfr-output-file)
                               gouvfr-dataset gouvfr-resource)))
    (timbre/error "First arg must by \"top250\" or \"gouvfr\""))
  (System/exit 0))
