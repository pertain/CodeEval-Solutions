;; pu.clj
;;
;; By William Ersing
;;
;; This is a programming challenge from CodeEval.com.
;; It reads in a file with each line containing a
;; string of words. The goal is to locate the
;; penultimate word (second from last) in each string
;; and print it to stdout.
;;
;; Example:
;;
;;  "I loaf and lean at my ease"
;;
;;    The penultimate word is: "my"

(require '[clojure.java.io :as io]
         '[clojure.string :as str])
(with-open [rdr (io/reader (second *command-line-args*))]
  (doseq [line (remove empty? (line-seq rdr))]
    (println (second (reverse (str/split line #"\s"))))))
