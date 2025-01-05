(defun llm-chat-search-completion ()
  "Search through LLM chat files and present completion candidates.
Shows backend, model info, filename, and size metadata.
Opens the selected file."
  (interactive)
  (let* ((search-dir (expand-file-name "~/Documents/llm-chats/"))
         (pattern (completing-read "Search LLM chats: " nil nil nil))
         (files (directory-files-recursively search-dir "\\.\\(org\\|md\\)$"))
         (matches))
    ;; Filter and search each file
    (dolist (file files)
      (let* ((filename (file-name-nondirectory file))
             (filename-match (string-match-p pattern filename))
             (content-match nil)
             (model "Unknown model")
             (backend "Unknown backend"))
        ;; Skip temporary/lock/hidden files
        (unless (or (string-prefix-p "." filename)      ; Hidden files
                    (string-prefix-p "#" filename)       ; Emacs lock files
                    (string-suffix-p "#" filename)       ; Emacs auto-save files
                    (string-suffix-p "~" filename)       ; Emacs backup files
                    (string-match-p "\\.#" filename))    ; Emacs symlink lock files
          ;; Extract model info from the gptel metadata and check content
          (with-temp-buffer
            (insert-file-contents file)
            ;; Try to find model info - try both formats
            (goto-char (point-min))
            (cond
             ;; .org format
             ((re-search-forward "^:GPTEL_MODEL:\\s-*\\(.*\\)$" nil t)
              (setq model (match-string 1)))
             ;; .md format
             ((re-search-forward "gptel-model: \"\\([^\"]+\\)\"" nil t)
              (setq model (match-string 1))))
            
            ;; Try to find backend info - try both formats
            (goto-char (point-min))
            (cond
             ;; .org format
             ((re-search-forward "^:GPTEL_BACKEND:\\s-*\\(.*\\)$" nil t)
              (setq backend (match-string 1)))
             ;; .md format
             ((re-search-forward "<!-- gptel--backend-name: \"\\([^\"]+\\)\" -->" nil t)
              (setq backend (match-string 1))))
            
            ;; Search for pattern match
            (goto-char (point-min))
            (while (and (not content-match) (search-forward-regexp pattern nil t))
              (setq content-match t)))
          ;; Add to matches if either filename or content matches
          (when (or filename-match content-match)
            (push (list file model backend) matches)))))
    ;; Provide completion candidates with metadata for matching files
    (if matches
        (let* ((candidates (mapcar (lambda (match)
                                     (let* ((file (nth 0 match))
                                            (model (nth 1 match))
                                            (backend (nth 2 match))
                                            (backend-str (propertize (format "%-20s" backend)
                                                                     'face 'shadow))
                                            (model-str (propertize (format "%-40s" model)
								   'face 'completions-annotations))
                                            (filename-str (propertize (file-name-nondirectory file)
                                                                      'face 'completions-common-part))
                                            (display-name (format "%s %s %s"
								  backend-str
								  model-str
								  filename-str)))
                                       (propertize display-name
                                                   'full-path file
                                                   'size (file-attribute-size (file-attributes file))
                                                   'display-size (format-file-size 
								  (file-attribute-size (file-attributes file)))
                                                   'model model
                                                   'backend backend)))
                                   (reverse matches)))
               (max-name-length (apply #'max (mapcar #'length candidates)))
               (completion-extra-properties
                `(:annotation-function
                  ,(lambda (cand)
                     (let ((padding (make-string 
                                     (- (+ max-name-length 4)
					(length cand))
                                     ?\s)))
                       (propertize (format "%s[%s]" 
                                           padding
                                           (get-text-property 0 'display-size cand))
                                   'face 'completions-annotations)))))
               (selected (completing-read "Select file to open: " candidates nil t)))
          (find-file (get-text-property 0 'full-path (car (member selected candidates)))))
      (message "No matching files found."))))

(defun format-file-size (size)
  "Format file SIZE in a human-readable way."
  (cond
   ((< size 1024) (format "%dB" size))
   ((< size (* 1024 1024)) (format "%.1fK" (/ size 1024.0)))
   ((< size (* 1024 1024 1024)) (format "%.1fM" (/ size (* 1024.0 1024.0))))
   (t (format "%.1fG" (/ size (* 1024.0 1024.0 1024.0))))))

(global-set-key (kbd "C-c l s") 'llm-chat-search-completion)
