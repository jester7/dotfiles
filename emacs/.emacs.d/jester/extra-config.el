;(setq org-startup-with-inline-images t)
;(setq org-hide-emphasis-markers t)

;; (defun my-dired-preview-to-the-bottom ()
;;   "My preferred `dired-preview-display-action-alist-function'."
;;   '((display-buffer-in-side-window)
;;     (side . bottom)
;;     (height . 0.3)))

;; (use-package dired-preview
;;   :ensure t
;;   :after dired
;;   :hook (dired-mode . dired-preview-mode)
;;   :config
;;   (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-bottom)
;;   (setq dired-preview-delay 0.0)
;;   (setq dired-preview-ignored-extensions-regexp
;;       (concat "\\."
;;               "\\(ds_store\\|mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
;;               "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
;;               "\\|iso\\|epub\\|pdf\\)")))


;; (setq ring-bell-function
;;       (lambda ()
;;         (let ((orig-fg (face-foreground 'mode-line)))
;;           (set-face-foreground 'mode-line "#F2804F")
;;           (run-with-idle-timer 0.1 nil
;;                                (lambda (fg) (set-face-foreground 'mode-line fg))
;;                                orig-fg))))

;;(setq frame-title-format "%b - emacs")

;;(setq frame-title-format (list "%b - " (getenv "USER") "@" (getenv "HOST")))
;; (setq frame-title-format (list "%b - %f "
;; 			       " ("  user-login-name "@" system-name ")"))

;; (if (buffer-modified-p) "❗" )

;; update this function to omit | at the end of the frame title

;; (defun generate-frame-title ()
;;     (let ((retval ""))
;;       (walk-window-tree (lambda (x) (setf retval (seq-concatenate  'string (buffer-name (window-buffer x))
;; 							      (if (> (car (buffer-line-statistics  (window-buffer x))) 200) "😰" )
;; 							     (if (buffer-modified-p (window-buffer x)) "✨")
;; 							     "   ~   "
;; 							     retval))))
;;       (setf (frame-parameter nil 'name) (substring retval 0 -7))
;;      retval))
;;   (setq-default frame-title-format '(:eval (generate-frame-title)))
