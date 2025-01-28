#!/usr/bin/env bb
(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

(def excluded-dirs
  #{"node_modules" "__pycache__" ".git" "target" "venv" "dist" "build"})

(defn expand-path [path]
  (-> path
      (fs/expand-home)
      (fs/normalize)
      (fs/absolutize)))

(defn parse-args [args]
  (loop [args args
         options {:gitignore? false
                  :ignore-patterns []
                  :dir "."}]
    (if (empty? args)
      options
      (let [arg (first args)]
        (cond
          (= "-g" arg) (recur (rest args) (assoc options :gitignore? true))
          (and (= "-i" arg) (second args)) (recur (drop 2 args)
                                                 (update options :ignore-patterns into 
                                                         (str/split (second args) #",")))
          (not (str/starts-with? arg "-")) (recur (rest args) (assoc options :dir arg))
          :else (do (println "Unknown option:" arg) (System/exit 1)))))))

;; (defn read-gitignore [dir]
;;   (let [gitignore (fs/file dir ".gitignore")]
;;     (when (fs/exists? gitignore)
;;       (->> (slurp gitignore)
;;            (str/split-lines)
;;            (remove #(or (str/starts-with? % "#")
;;                         (str/blank? %)))))))

(defn read-gitignore [dir]
  (let [gitignore (fs/file dir ".gitignore")]
    (when (fs/exists? gitignore)
      (->> (slurp gitignore)
           (str/split-lines)
           (remove #(or (str/starts-with? % "#") (str/blank? %)))
           ;; Convert to glob patterns
           (map #(cond-> (str/replace % #"/$" "")
                   (not (str/includes? % "/")) (str "**/")))
           ))))

(defn path-matcher [pattern]
  (.. (java.nio.file.FileSystems/getDefault)
      (getPathMatcher (str "glob:" pattern))))

;; (defn should-exclude? [file root-dir gitignore-patterns ignore-patterns]
;;   (or
;;    ;; Existing exclusions
;;    (and (fs/directory? file) (fs/hidden? file))
;;    ;; (contains? excluded-dirs (fs/file-name file))
;;    (str/starts-with? (fs/file-name file) "#")
;;    (str/starts-with? (fs/file-name file) ".#")
   
;;    ;; Gitignore patterns
;;    (let [rel-path (-> (.relativize root-dir file)
;;                       .toString)
;;          path-to-match (fs/path rel-path)]
;;      (or (some #(-> % path-matcher (.matches path-to-match)) gitignore-patterns)
;;          (some #(-> % path-matcher (.matches path-to-match)) ignore-patterns)))))
(defn should-exclude? [file root-dir gitignore-patterns ignore-patterns]
  (or
   (and (fs/directory? file) (fs/hidden? file))
   (let [rel-path (-> (.relativize root-dir file)
                      .toString)
         path-to-match (fs/path rel-path)]
     (or (some #(let [m (path-matcher %)]
                  ;; (println "Checking:" rel-path "against pattern:" %)
                  (.matches m path-to-match))
               gitignore-patterns)
         (some #(let [m (path-matcher %)]
                  (.matches m path-to-match))
               ignore-patterns)))))

(defn print-tree [dir root-dir level gitignore-patterns ignore-patterns]
  (let [indent (str/join (repeat level "  "))]
    (doseq [f (sort (fs/list-dir dir))]
      (when-not (should-exclude? f root-dir gitignore-patterns ignore-patterns)
        (println (str indent "├── " (fs/file-name f)))
        (when (fs/directory? f)
          (print-tree f root-dir (inc level) gitignore-patterns ignore-patterns))))))

(let [args *command-line-args*
      {:keys [dir gitignore? ignore-patterns]} (parse-args args)
      target-dir (expand-path dir)
       gitignore-pats (when gitignore? (read-gitignore target-dir))
      ignore-pats (map #(cond-> (str/replace % #"/$" "")
                           (not (str/includes? % "/")) (str "**/"))
                      ignore-patterns)]
  (println (str "Tree for: " target-dir))
  (println gitignore-pats)
  (print-tree target-dir target-dir 0 
            (keep #(str "**/" %) gitignore-pats) ; Match in any directory
            (keep #(str "**/" %) ignore-pats))
  )

