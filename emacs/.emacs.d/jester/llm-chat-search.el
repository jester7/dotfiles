(defcustom llm-chat-search-root-dir (expand-file-name "~/Documents/llm-chats/")
  "Root directory for LLM chat files search."
  :type 'directory
  :group 'gptel)

(defun llm-chat-search-format-file-size (size)
  "Format file size in a human-readable way."
  (cond
   ((< size 1024) (format "%dB" size))
   ((< size (* 1024 1024)) (format "%.1fK" (/ size 1024.0)))
   ((< size (* 1024 1024 1024)) (format "%.1fM" (/ size (* 1024.0 1024.0))))
   (t (format "%.1fG" (/ size (* 1024.0 1024.0 1024.0))))))

(defun llm-chat-search-completion ()
  "Search through LLM chat files, presenting completion candidates
with backend, model info, filename, last modified time, and size metadata,
sorted by modification time. Shows all files if search string is empty."
  (interactive)
  (let* ((search-dir llm-chat-search-root-dir)
         (pattern (completing-read "Search LLM chats (empty for all): " nil nil nil))
         (files (directory-files-recursively search-dir "\\.\\(org\\|md\\)$"))
         (matches))
    ;; Filter and search each file
    (dolist (file files)
      (let* ((filename (file-name-nondirectory file))
             (filename-match (if (string-empty-p pattern)
				 t  ; Match all files if pattern is empty
                               (string-match-p pattern filename)))
             (content-match nil)
             (model "Unknown model")
             (backend "Unknown backend")
             (file-attrs (file-attributes file))  ; Call file-attributes once
             (mtime (file-attribute-modification-time file-attrs))
             (size (file-attribute-size file-attrs)))
        ;; Skip temporary/lock/hidden files
        (unless (or (string-prefix-p "." filename)
                    (string-prefix-p "#" filename)
                    (string-suffix-p "#" filename)
                    (string-suffix-p "~" filename)
                    (string-match-p "\\.#" filename))
          ;; Extract model info and check content
          (with-temp-buffer
            (insert-file-contents file)
            ;; Try to find model info - try both formats
            (goto-char (point-min))
            (cond
             ((re-search-forward "^:GPTEL_MODEL:\\s-*\\(.*\\)$" nil t)
              (setq model (match-string 1)))
             ((re-search-forward "gptel-model: \"\\([^\"]+\\)\"" nil t)
              (setq model (match-string 1))))
            
            ;; Try to find backend info - try both formats
            (goto-char (point-min))
            (cond
             ((re-search-forward "^:GPTEL_BACKEND:\\s-*\\(.*\\)$" nil t)
              (setq backend (match-string 1)))
             ((re-search-forward "<!-- gptel--backend-name: \"\\([^\"]+\\)\" -->" nil t)
              (setq backend (match-string 1))))
            
            ;; Search for pattern match if pattern isn't empty
            (when (not (string-empty-p pattern))
              (goto-char (point-min))
              (while (and (not content-match) (search-forward-regexp pattern nil t))
                (setq content-match t))))
          ;; Add to matches if either filename or content matches
          (when (or filename-match content-match)
            (push (list file model backend mtime size) matches)))))
    ;; (message "Before sorting:")
    ;; (dolist (match matches)
    ;;   (message "File: %s Time: %s Float-time: %f" 
    ;;            (file-name-nondirectory (nth 0 match))
    ;;            (format-time-string "%Y-%m-%d %H:%M" (nth 3 match))
    ;;            (float-time (nth 3 match))))

    ;; Sort matches by modification time, newest first
    (setq matches (sort matches
			(lambda (a b)
			  (> (float-time (nth 3 a)) 
                             (float-time (nth 3 b))))))

    ;; After sorting, print again to verify
    ;; (message "After sorting:")
    ;; (dolist (match matches)
    ;;   (message "File: %s Time: %s Float-time: %f" 
    ;;            (file-name-nondirectory (nth 0 match))
    ;;            (format-time-string "%Y-%m-%d %H:%M" (nth 3 match))
    ;;            (float-time (nth 3 match))))

    ;; Provide completion candidates with metadata for matching files
    (if matches
	(let* ((candidates (mapcar (lambda (match)
                                     (let* ((file (nth 0 match))
					    (model (nth 1 match))
					    (backend (nth 2 match))
					    (mtime (nth 3 match))
					    (size (nth 4 match))
					    (backend-str (propertize (format "%-20s" backend)
								     'face 'shadow))
					    (model-str (propertize (format "%-45s" model)
								   'face 'completions-annotations))
					    (filename-str (propertize (format "%-30s" (file-name-nondirectory file))
								      'face 'completions-common-part))
					    (time-str (format-time-string "%Y-%m-%d %H:%M" mtime))
					    (display-name (format "%s %s %s"
								  backend-str
								  model-str
								  filename-str)))
                                       (propertize display-name
						   'full-path file
						   'size size
						   'display-size
						   (llm-chat-search-format-file-size
						    size)
						   'mtime time-str
						   'model model
						   'backend backend)))
				   matches))
               (max-name-length (apply #'max (mapcar #'length candidates)))
               (completion-extra-properties
		`(:annotation-function
		  ,(lambda (cand)
                     (let ((padding (make-string 
                                     (- (+ max-name-length 4)
					(length cand))
                                     ?\s)))
                       (propertize (format "%s%s [%s]" 
					   padding
					   (get-text-property 0 'mtime cand)
					   (get-text-property 0 'display-size cand))
				   'face 'completions-annotations)))))
               ;; Create completion table that preserves order
               (table (lambda (string pred action)
			(if (eq action 'metadata)
			    '(metadata (display-sort-function . identity)
                                       (cycle-sort-function . identity))
			  (complete-with-action action candidates string pred))))
               (selected (completing-read "Select file to open: " table nil t)))
	  (let ((buf
		 (find-file
		  (get-text-property 0 'full-path (car (member selected candidates))))))
	    (with-current-buffer buf
	      (gptel-mode)
	      (when (derived-mode-p 'org-mode)
		(org-cycle))
	      (set-buffer-modified-p nil)
	      (goto-char (point-max)))))
      (message "No matching files found."))))

(global-set-key (kbd "C-c l s") 'llm-chat-search-completion)
