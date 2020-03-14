(ns utils
  (:require [clojure.string]
            [semantic-csv.core :as csv]
            [clj-http.client :as http]))

(def config
  {:path-driver          "/usr/lib/chromium-browser/chromedriver"
   :path-browser         "/usr/bin/chromium-browser"
   :wait                 1
   :data-path            "data/"
   :screenshots          "screenshots/"
   :gouvfr-init-file     "gouvfr-init.csv"
   :gouvfr-output-file   "gouvfr.csv"
   :gouvfr-raw-text-file "gouvfr-raw.txt"
   :top250-init-file     "top250-init.csv"
   :top250-output-file   "top250.csv"
   :top250-raw-csv-file  "a23f3995-fcfb-414c-ae3d-82adb90c07cc"
   :http-get-opts        {:insecure?          true
                          :cookie-policy      :standard
                          :socket-timeout     2000
                          :connection-timeout 2000
                          :max-redirects      3}
   :chromium-opts        {:path-driver  (:path-driver config)
                          :path-browser (:path-browser config)
                          ;; :load-strategy :normal ;; FIXME: what does it change?
                          :headless     true
                          :dev
                          {:perf
                           {:level      :all
                            :network?   true
                            :page?      true
                            :interval   1000
                            :categories [:devtools]}}}})

(defn path [k] (str (:data-path config) (k config)))

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))

(defn top250-init []
  (csv/spit-csv
   (path :top250-init-file)
   (distinct-by
    :URL
    (map #(select-keys % [:Id :Démarche :URL])
         (csv/slurp-csv (path :top250-raw-csv-file))))))

(def gouvfr-domains
  (distinct
   (clojure.string/split-lines
    (slurp (path :gouvfr-raw-text-file)))))

(defn gouvfr-init []
  (let [valid-domains (atom nil)]
    (doseq [d gouvfr-domains]
      (let [dp    (str "http://" d)
            resp  (try (http/get dp (:http-get-opts config))
                       (catch Exception _ nil))
            redir (last (:trace-redirects resp))]
        (when (= (:status resp) 200)
          (swap! valid-domains conj (or redir dp)))))
    (csv/spit-csv
     (path :gouvfr-init-file)
     (map (fn [d] {:URL d}) @valid-domains))))

