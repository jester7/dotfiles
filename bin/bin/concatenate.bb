#!/usr/bin/env bb

(require '[clojure.java.io :as io])

;; Get command-line arguments
(def args *command-line-args*)

;; Function to display usage information
(defn usage []
  (println "Usage: concat_files.clj <directory> <output_file.txt>")
  (System/exit 1))

;; Main script logic
(if (< (count args) 2)
  (usage)
  (let [[dir output-file] args
        dir-file (io/file dir)]
    ;; Ensure the directory exists and is a directory
    (if (not (.isDirectory dir-file))
      (do
        (println "Error: The specified path is not a directory.")
        (System/exit 1))
      (let [files (filter #(.isFile %) (file-seq dir-file))]
        (with-open [w (io/writer output-file)]
          (doseq [f files]
            (let [filename (.getName f)]
              ;; Write the begin separator with the filename
              (.write w (str "===== Begin " filename " =====\n"))
              ;; Copy the file contents to the output file
              (io/copy (io/reader f) w)
              ;; Write the end separator with the filename
              (.write w (str "\n===== End " filename " =====\n\n")))))))))
