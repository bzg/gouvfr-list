(ns utils
  (:require [clojure.string]
            [semantic-csv.core :as csv]
            [taoensso.timbre :as timbre]
            [clj-http.client :as http]
            [clojure.set]
            [java-time :as t])
  (:import [java.net URLEncoder]))

(def config
  {:log-file             "log.txt"
   :driver-wait          1
   :driver-timeout       10000
   :data-path            "data/"
   :screenshots-rel-path "screenshots/"
   :gouvfr-init-file     "gouvfr-init.csv"
   :gouvfr-output-file   (format "gouvfr-%s.csv"
                                 (t/format "MM-dd-YYY" (t/local-date)))
   :gouvfr-raw-text-file "gouvfr-raw.txt"
   :top250-init-file     "top250-init.csv"
   :top250-output-file   (format "tdb-demarches-phares-informations-supplementaires-%s.csv"
                                 (t/format "YYYYMMdd" (t/local-date)))
   :top250-raw-csv-file  "a23f3995-fcfb-414c-ae3d-82adb90c07cc"
   :http-params          {:insecure?          true
                          :cookie-policy      :standard
                          :socket-timeout     2000
                          :connection-timeout 2000
                          :max-redirects      3}
   :chromium-opts        {:path-driver "/usr/lib/chromium-browser/chromedriver"
                          ;; :load-strategy :normal ;; FIXME: what does it change?
                          :headless    true
                          :dev
                          {:perf
                           {:level      :all
                            :network?   true
                            :page?      false
                            :interval   1000
                            :categories [:devtools
                                         :devtools.network]}}}})

(defn path [k] (str (:data-path config) (k config)))

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))

(defn url-domain-only [s]
  (clojure.string/replace s #"^https?://([^/]+)/.*$" "$1"))

(defn url-no-protocol [s]
  (clojure.string/replace s #"^https?://(.+[^/])/?$" "$1"))

(defn url-encode
  "Returns an UTF-8 URL encoded version of the given string."
  [^String unencoded]
  (URLEncoder/encode unencoded "UTF-8"))

(defn string-replace-newline [s]
  (-> s
      (clojure.string/replace #"(\r\n|\n|\r)+" " ")
      (clojure.string/replace #"  +" " ")
      (clojure.string/trim)))

(defn top250-csv-fields-to-fr [m]
  (map #(clojure.set/rename-keys
         % {:tags            :balises
            :using-ga?       :utilise-google-analytics
            :is-secure?      :certificat-valide
            :keywords        :mots-clefs
            :title           :titre
            :requests-number :nombre-de-requetes
            :content-length  :taille}) m))

(defn top250-init []
  (timbre/info (str "Initializing " (path :top250-init-file)))
  (csv/spit-csv
   (path :top250-init-file)
   (distinct-by
    :URL
    (map #(select-keys % [:Id :Démarche :URL])
         (csv/slurp-csv (path :top250-raw-csv-file))))))

(def gouvfr-domains
  (clojure.string/split-lines
   (slurp (path :gouvfr-raw-text-file))))

(defn gouvfr-init []
  (timbre/info (str "Initializing " (path :gouvfr-init-file)))
  (let [valid-domains (atom nil)]
    (doseq [d gouvfr-domains]
      (let [dp    (str "http://" d)
            resp  (try (http/get dp (:http-params config))
                       (catch Exception _ nil))
            redir (last (:trace-redirects resp))]
        (when (= (:status resp) 200)
          (swap! valid-domains conj {:URL (or redir dp)}))))
    (csv/spit-csv
     (path :gouvfr-init-file)
     (distinct-by #(let [r (clojure.string/replace % #"/$" "")] r)
                  @valid-domains))))
