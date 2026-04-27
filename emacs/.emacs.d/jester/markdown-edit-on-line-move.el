;;; -*- lexical-binding: t; -*-

;; got original idea from:
;; https://emacs.stackexchange.com/questions/47632/how-do-i-make-markdown-or-org-mode-hide-formatting-characters-until-i-edit

(defvar markdown-hide-markup)
(defvar markdown-inline-image-overlays)
(defvar font-lock-beginning-of-syntax-function)
(declare-function markdown-display-inline-images "markdown-mode")
(declare-function markdown-toggle-markup-hiding "markdown-mode" (&optional arg))

;; the default font-lock-fontify-block prints an error message
;; for lines that are plain text possibly, TODO investigate further
(defun jester/markdown-font-lock-fontify-block (&optional arg)
  "Fontify the block around point without printing error messages.

This is adapted from `font-lock-fontify-block'."
  (interactive "P")
  (let ((cursor-sensor-inhibit t) font-lock-beginning-of-syntax-function
	deactivate-mark)
    ;; Make sure we have the right `font-lock-keywords' etc.
    (if (not font-lock-mode) (font-lock-set-defaults))
    (save-excursion
      (save-match-data
	(condition-case nil
	    (if (or arg (not font-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (font-lock-fontify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall font-lock-mark-block-function)
	      (font-lock-fontify-region (point) (mark)))
	  (error nil))))))

(defconst jester/markdown-edit-max-buffer-size 200000
  "Maximum buffer size for automatic markdown edit-on-line-move setup.")

(defvar-local jester/markdown-current-line nil
  "Cons of current line bounds whose markdown markup is unhidden.")

(defvar-local jester/markdown-fontify-timer nil
  "Idle timer used to delay markdown line refontification.")

(defvar-local jester/markdown-edit-on-line-move-enabled nil
  "Non-nil when markdown edit-on-line-move has been installed.")

(defun jester/markdown-current-line-bounds ()
  "Return the start and end positions for the line at point."
  (cons (line-beginning-position) (line-beginning-position 2)))

(defun jester/markdown-unhide-current-line (limit)
  "Remove hiding properties from the active markdown line up to LIMIT."
  (let ((start (and jester/markdown-current-line
                    (max (point) (car jester/markdown-current-line))))
        (end (and jester/markdown-current-line
                  (min limit (cdr jester/markdown-current-line)))))
    (when (and start end (< start end))
      (remove-text-properties start end
                            '(invisible nil display nil composition nil))
      (goto-char limit)
      t)))

(defun jester/markdown-refontify-line (line)
  "Refontify LINE, a cons of line bounds."
  (when line
    (save-excursion
      (goto-char (car line))
      (jester/markdown-font-lock-fontify-block 1))))

(defun jester/markdown-refontify-on-line-move ()
  "Refresh markdown hiding when point enters a different line."
  (let ((line (jester/markdown-current-line-bounds)))
    (unless (equal line jester/markdown-current-line)
      (let ((previous-line jester/markdown-current-line))
        (setq jester/markdown-current-line line)
        (jester/markdown-refontify-line previous-line)
        (jester/markdown-refontify-line line)))))

(defun jester/markdown-run-debounced-refontify (buffer)
  "Run delayed markdown refontification in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq jester/markdown-fontify-timer nil)
      (when (derived-mode-p 'markdown-mode)
        (jester/markdown-refontify-on-line-move)))))

(defun jester/markdown-debounced-refontify ()
  "Schedule a delayed markdown refontification for the current buffer."
  (when jester/markdown-fontify-timer
    (cancel-timer jester/markdown-fontify-timer))
  (setq jester/markdown-fontify-timer
        (run-with-idle-timer 0.1 nil
                             #'jester/markdown-run-debounced-refontify
                             (current-buffer))))

(defun jester/markdown-clear-cache (&rest _)
  "Clear cached markdown line bounds after buffer changes."
  (setq jester/markdown-current-line nil))

(defun jester/markdown-edit-on-line-move ()
  "Expose markdown markup on the current line while editing."
  (when (and (< (buffer-size) jester/markdown-edit-max-buffer-size)
             (not jester/markdown-edit-on-line-move-enabled))
    (setq jester/markdown-edit-on-line-move-enabled t)
    (unless markdown-hide-markup
      (markdown-toggle-markup-hiding 1))
    (when (and (display-images-p)
               (not markdown-inline-image-overlays))
      (ignore-errors
        (markdown-display-inline-images)))
    (font-lock-add-keywords nil '((jester/markdown-unhide-current-line)) t)
    (add-hook 'post-command-hook #'jester/markdown-debounced-refontify nil t)
    (add-hook 'before-change-functions #'jester/markdown-clear-cache nil t)))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'jester/markdown-edit-on-line-move))

;; (add-hook 'org-mode-hook #'jester/markdown-edit-on-line-move)
