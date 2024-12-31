;; adapted from:
;; https://emacs.stackexchange.com/questions/47632/how-do-i-make-markdown-or-org-mode-hide-formatting-characters-until-i-edit

;; the default font-lock-fontify-block prints an error message
;; for lines that are plain text possibly, TODO investigate further
(defun my/font-lock-fontify-block (&optional arg)
  "taken from emacs source code, removed error message printing"
  (interactive "P")
  (let ((cursor-sensor-inhibit t) font-lock-beginning-of-syntax-function
	deactivate-mark)
    ;; Make sure we have the right `font-lock-keywords' etc.
    (if (not font-lock-mode) (font-lock-set-defaults))
    (save-excursion
      (save-match-data
	(condition-case error-data
	    (if (or arg (not font-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (font-lock-fontify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall font-lock-mark-block-function)
	      (font-lock-fontify-region (point) (mark)))
	  ((error quit) nil))))))

(defvar my/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(defvar my/last-point nil
  "Last point where the line was unhidden.")

(make-variable-buffer-local 'my/last-point)

(make-variable-buffer-local 'my/current-line)

(defun my/unhide-current-line (limit)
  "Optimized font-lock function"
  (let ((start (max (point) (car my/current-line)))
        (end (min limit (cdr my/current-line))))
    (when (< start end)
      (remove-text-properties start end
                            '(invisible nil display nil composition nil))
      (goto-char limit)
      t)))

(defun my/refontify-on-line-move ()
  "Post-command-hook with optimizations"
  (when (or (not (eq (point) my/last-point))
            (not (equal (cons (line-beginning-position)
                            (line-beginning-position 2))
                      my/current-line)))
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2)))
      ;; Update current line bounds
      (setq my/current-line (cons start end))
      ;; Fontify previous location if needed
      (when (and my/last-point
                 (not (eq (point) my/last-point)))
        (save-excursion
          (goto-char my/last-point)
          (my/font-lock-fontify-block 1)))
      ;; Update last point and fontify current line
      (setq my/last-point (point))
      (my/font-lock-fontify-block 1))))

(defvar-local my/fontify-timer nil)
(defun my/debounced-refontify ()
  (when my/fontify-timer
    (cancel-timer my/fontify-timer))
  (setq my/fontify-timer
        (run-with-idle-timer 0.1 nil #'my/refontify-on-line-move)))

(defun my/markdown-edit-on-line-move ()
  "Install with buffer size check"
  (when (< (buffer-size) 200000) ; Only enable for files under 100KB
    (markdown-toggle-markup-hiding)
    (markdown-toggle-inline-images)
    (font-lock-add-keywords nil '((my/unhide-current-line)) t)
    (add-hook 'post-command-hook #'my/debounced-refontify nil t)))

(defun my/clear-markdown-cache ()
  (setq my/last-point nil)
  (setq my/current-line '(0 . 0)))

(add-hook 'before-change-functions
          (lambda (&rest _)
            (when (> (- (point-max) (point-min)) 1000)
              (my/clear-markdown-cache))))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'my/markdown-edit-on-line-move))

;; (add-hook 'org-mode-hook #'my/markdown-edit-on-line-move)
