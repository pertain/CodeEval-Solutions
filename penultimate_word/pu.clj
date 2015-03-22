(require '[clojure.java.io :as io]
         '[clojure.string :as str])
(with-open [rdr (io/reader (second *command-line-args*))]
  (doseq [line (remove empty? (line-seq rdr))]
    (println (second (reverse (str/split line #"\s"))))))
