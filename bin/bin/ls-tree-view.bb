#!/usr/bin/env bb
(require '[babashka.fs :as fs])

(defn expand-path [path]
  (-> path
      (fs/expand-home)  ; Expands ~ to home directory
      (fs/normalize)    ; Normalizes path with ./ and ../
      (fs/absolutize))) ; Converts to absolute path

(defn print-tree [dir level]
  (let [indent (apply str (repeat level "  "))]
    (doseq [f (sort (fs/list-dir dir))]
      (when-not (or (and (fs/directory? f) (fs/hidden? f))
                    (= (fs/file-name f) "node_modules")
                    (.startsWith (fs/file-name f) "#")
                    (.startsWith (fs/file-name f) ".#"))
        (println (str indent "├── " (fs/file-name f)))
        (when (fs/directory? f)
          (print-tree f (inc level)))))))

(let [target-dir (or (first *command-line-args*) ".")
      expanded-dir (expand-path target-dir)]
  (println (str "Tree for: " expanded-dir))
  (print-tree (fs/file expanded-dir) 0))
