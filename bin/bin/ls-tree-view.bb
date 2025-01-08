#!/usr/bin/env bb
(require '[babashka.fs :as fs])

(def excluded-dirs
  #{"node_modules"         ; npm modules
    "__pycache__"         ; Python cache
    ".git"                ; Git directory
    "target"              ; Common build directory
    "venv"                ; venv
    "dist"                ; Distribution directory
    "build"})             ; Build directory

(defn expand-path [path]
  (-> path
      (fs/expand-home)    ; Expands ~ to home directory
      (fs/normalize)      ; Normalizes path with ./ and ../
      (fs/absolutize)))   ; Converts to absolute path

(defn should-exclude? [file]
  (or
   ;; Exclude hidden directories
   (and (fs/directory? file) (fs/hidden? file))
   ;; Exclude specific directory names
   (contains? excluded-dirs (fs/file-name file))
   ;; Exclude emacs temporary files
   (.startsWith (fs/file-name file) "#")
   (.startsWith (fs/file-name file) ".#")))

(defn print-tree [dir level]
  (let [indent (apply str (repeat level "  "))]
    (doseq [f (sort (fs/list-dir dir))]
      (when-not (should-exclude? f)
        (println (str indent "├── " (fs/file-name f)))
        (when (fs/directory? f)
          (print-tree f (inc level)))))))

(let [target-dir (or (first *command-line-args*) ".")
      expanded-dir (expand-path target-dir)]
  (println (str "Tree for: " expanded-dir))
  (print-tree (fs/file expanded-dir) 0))
