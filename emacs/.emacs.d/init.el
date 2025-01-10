(use-package ef-themes
  :ensure t
  :defer nil
  :init
  (if (display-graphic-p)
      (load-theme 'ef-dark t)
    (load-theme 'ef-elea-dark t)))

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(require 'package)
;; (require 'quelpa-use-package)

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar my-package-archive '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (defvar my-package-archive '("melpa" . "https://melpa.org/packages/"))

(defun my-add-and-refresh ()
  "Add melpa stable to package-archives and refresh contents."
  (interactive)
  ;; if not already added, add
  (unless (assoc (car my-package-archive) package-archives)
    (add-to-list 'package-archives my-package-archive t))
  (package-refresh-contents))

;; bind to C-c u
(global-set-key (kbd "C-c r") 'my-add-and-refresh)

(blink-cursor-mode t)

(set-frame-parameter nil 'alpha-background 70)
(add-to-list 'default-frame-alist '(alpha-background . 70))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 99))

(use-package org
  :ensure t
  :defer t
  :config
  (require 'org-tempo)
  (setq org-support-shift-select t)
  :hook
  (org-mode . visual-line-mode))

(use-package emacs
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (ns-auto-hide-menu-bar nil)
  (delete-selection-mode t)
  (display-line-numbers-type 'relative)
  (global-hl-line-mode nil)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  (setq inhibit-compacting-font-caches t)
  (setq find-file-visit-truename t)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%d-%m-%Y %H:%M")
  (display-time)
  (when (display-graphic-p)
    (context-menu-mode)))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t)
  ;;undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :init
  (global-undo-tree-mode))

(push "#" completion-ignored-extensions)
(push ".DS_Store" completion-ignored-extensions)



;; (define-skeleton clojure-skeleton-defn
;;   "Insert a clojure function definition with a blank docstring"
;;   > "(defn " _ " []" \n
;;   -2 "\"\"" \n ")"
;;   -1 )

;; (define-skeleton clojure-skeleton-defn-let
;;   "Insert a clojure function definition with a blank docstring and a let block"
;;   > "(defn " _ " []" \n
;;   -2 "\"\"" \n
;;   -1 "(let []" \n
;;   -1 ")" \n ")"
;;   -1 )

;; (define-abbrev clojure-mode-abbrev-table ";defn"
;;   "" 'clojure-skeleton-defn)

;; (define-abbrev clojure-mode-abbrev-table ";defnlet"
;;   "" 'clojure-skeleton-defn-let)


;;(quietly-read-abbrev-file)
;;(save-abbrevs 'silent)

(use-package vertico
  :ensure t
  :defer t
  :custom
  (vertico-cycle t)
  (vertico-mouse-mode t)
  (vertico-reverse-mode t)
  (vertico-grid-mode nil)
  (vertico-resize t)
  ;; (vertico-scroll-margin 1)
  ;; (resize-mini-windows nil)
  :init
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :defer t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :defer t
  :config
  (setq orderless-matching-styles '(orderless-initialism
                                    orderless-literal
                                    orderless-regexp))
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :defer t)

(global-set-key (kbd "M-s b") 'consult-buffer)
(global-set-key (kbd "C-M-s-B") 'consult-buffer)
(global-set-key (kbd "M-s-x s") 'ace-swap-window)
(global-set-key (kbd "C-x 4 4") 'ace-delete-window)
(global-set-key (kbd "C-x 5 x") 'delete-frame-killing-buffer)
(global-set-key (kbd "M-s-o") 'other-frame)
(global-set-key (kbd "M-s-p") 'previous-buffer)
(global-set-key (kbd "M-s-n") 'next-buffer)
;; (global-unset-key (kbd "<down-mouse-3>"))
(global-unset-key (kbd "<mouse-4>"))
;; (global-set-key (kbd "<down-mouse-3>") 'previous-buffer)
(global-set-key (kbd "<mouse-5>") 'next-buffer)
(setq-default cursor-type '(bar . 2))
;; (setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'hollow)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :mode ("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)
	 ("\\.cljc\\'" . clojurec-mode)
	 ("\\.edn\\'" . clojure-mode)
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (clojure-mode . smartscan-mode))

(defun jester/cider-quit-delete-frame ()
  (interactive)
  ;; find the frame that has the cider-repl buffer
  (let* ((cider-repl-buffer (get-buffer (matches-a-buffer-name "\*cider")))
	(cider-repl-frame (get-buffer-window cider-repl-buffer)))
    (if cider-repl-frame
	(delete-frame (window-frame cider-repl-frame))))
  (cider-quit))

(defun jester/cider-quit-delete-window ()
  (interactive)
  (let* ((cider-repl-buffer (get-buffer (matches-a-buffer-name "\*cider")))
         (cider-repl-window (get-buffer-window cider-repl-buffer)))
    (when cider-repl-window
      (delete-window cider-repl-window)))
  (cider-quit))

(use-package cider
  :ensure t
  :defer t
  :bind (:map cider-repl-mode-map
	 ("C-c s-q" . jester/cider-quit-delete-window)
	 ("C-c s-s" . cider-connect-sibling-cljs)
	 ("C-c s-r" . cider-run))
  :hook (cider-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (cider-mode . (lambda ()
                         (add-hook 'after-save-hook 'cider-load-buffer nil 'make-it-local)))
  :custom
  (cider-font-lock-dynamically '(macro core function var)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist '(csharp python javascript cpp c)))


;; this fixes a problem where v0.20.4 of this grammar blows up with emacs
(defvar genehack/tsx-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'tsx
   :ts-mode 'tsx-ts-mode
   :remap '(typescript-tsx-mode)
   :requires 'typescript
   :url "https://github.com/tree-sitter/tree-sitter-typescriptx"
   :revision "v0.20.3"
   :source-dir "tsx/src"
   :ext "\\.tsx\\'")
  "Recipe for libtree-sitter-tsx.dylib")
(add-to-list 'treesit-auto-recipe-list genehack/tsx-treesit-auto-recipe)

(defvar genehack/typescript-treesit-auto-recipe
  (make-treesit-auto-recipe
   :lang 'typescript
   :ts-mode 'typescript-ts-mode
   :remap 'typescript-mode
   :requires 'tsx
   :url "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "v0.20.3"
   :source-dir "typescript/src"
   :ext "\\.ts\\'")
  "Recipe for libtree-sitter-typescript.dylib")
(add-to-list 'treesit-auto-recipe-list genehack/typescript-treesit-auto-recipe)


(setq tab-always-indent 'complete)

(use-package csharp-ts-mode
  :mode ("\\.cs\\'" . csharp-ts-mode) ; Associate .cs files with csharp-ts-mode
  :hook
  ((csharp-ts-mode . (lambda ()
		       (electric-indent-mode nil)
                       (corfu-mode -1)
		       (company-mode 1))))
  (csharp-ts-mode . subword-mode)
  (csharp-ts-mode . superword-mode)
  (csharp-ts-mode . smartscan-mode))

(fset #'jsonrpc--log-event #'ignore)
(setopt use-short-answers t)


;; (use-package eldoc-box
;;   :ensure t
;;   :defer t
;;   :hook
;;   (eglot-managed-mode . eldoc-box-hover-at-point-mode))

;;(use-package editorconfig

;; (use-package company 
;;   :ensure t
;;   :defer t
;;   :hook
;;   (c-ts-mode . company-mode)
;;   (csharp-ts-mode . company-mode)
;;   (python-ts-mode . company-mode)
;;   :config
;;   (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
;;   (define-key company-active-map (kbd "<escape>") #'company-abort))

(use-package iedit
  :ensure t)

(use-package magit
  :ensure t
  :defer t)

(use-package magit-section
  :ensure t
  :defer t)

(use-package diff-hl
  :ensure t
  ;; :custom
  ;; (diff-hl-draw-borders nil)
  :hook
  (prog-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (unless (window-system) (diff-hl-margin-mode))
  (diff-hl-flydiff-mode)
  (global-diff-hl-show-hunk-mouse-mode))


;; from https://git.sr.ht/~ashton314/emacs-bedrock/tree/main/item/init.el
;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

(setopt completions-group t)
(setopt completion-auto-select 'second-tab)
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; (setopt column-number-mode t)
(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
;;(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

(setq isearch-lazy-count t)
(setq isearch-allow-scroll t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " (%s/%s)")
(setq search-whitespace-regexp ".*?")



(setq mouse-wheel-tilt-scroll t)
(setq scroll-conservatively 101)
;; (setq pixel-scroll-precision-use-momentum nil)
;; (pixel-scroll-precision-mode)

(use-package fast-scroll
  :ensure t)

(add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
(add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
(fast-scroll-config)
(fast-scroll-mode 1)

(defun jester/random-background-image ()
  "Select a random background image from ~/Documents/emacs-backgrounds/"
  (let* ((image-dir "~/Documents/emacs-backgrounds")
         (images (directory-files (expand-file-name image-dir) t "\\.\\(png\\|jpg\\|gif\\|jpeg\\)$"))
         (chosen (when images (nth (random (length images)) images))))
    (or chosen "~/Documents/black-clover-emacs-banner-2.png"))) ; fallback image if directory is empty


(use-package dashboard
  :ensure t
  :custom-face
  (dashboard-heading ((t (:height 1.0 :weight bold))))
  (dashboard-items-face ((t (:height 1.0 :weight normal))))
  (dashboard-no-items-face ((t (:family "Ingra" :height 1.0 :weight regular))))
  (dashboard-footer ((t (:family "Ingra" :height 1.0 :weight medium))))
  :config
  (dashboard-setup-startup-hook)
  :hook
  ;; disable blinking cursor in dashboard mode
  (dashboard-mode . (lambda ()
		      (setq-local blink-cursor-mode nil)))
  :custom  
  (dashboard-startup-banner (jester/random-background-image))
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  ;; (dashboard-footer-messages (list			       
  ;; 			      (shell-command-to-string "curl -f -s 'http://wttr.in/Monterrey,Mexico?format=2'")))
  ;; (dashboard-footer-icon nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-items '((agenda . 12)
		     (recents . 6)
		     (projects . 6)
		     (bookmarks . 6))))

;; (defvar fetch-weather-timeout-seconds 2
;;   "Number of seconds before the weather fetch times out.")

;; (defun fetch-weather-for-dashboard ()
;;   (let ((start-time (current-time)))
;;     (url-retrieve
;;      "http://wttr.in/Monterrey,Mexico?format=2"
;;      (lambda (status)
;;        (if (or (plist-get status :error)
;;                (> (float-time (time-subtract (current-time) start-time)) fetch-weather-timeout-seconds))
;;            (setq dashboard-footer-messages '("Weather info unavailable"))
;;          (goto-char url-http-end-of-headers)
;;          (setq dashboard-footer-messages
;;                (list (buffer-substring-no-properties (point) (point-max)))))
;;        (dashboard-refresh-buffer)))))

;; ;; Fetch weather asynchronously with timeout when Emacs starts
;; (add-hook 'emacs-startup-hook 'fetch-weather-for-dashboard)

(global-prettify-symbols-mode t)

(use-package ns-auto-titlebar
  :ensure t
  :defer t)

(use-package all-the-icons-completion
  :ensure t
  :defer t)

(use-package telega
  :ensure t
  :defer t)

(all-the-icons-completion-mode)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

(use-package dired-x
  :ensure nil
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*"))
  (setq dired-omit-files (concat dired-omit-files "\\|\\.DS_Store$"))
  (add-to-list 'dired-omit-extensions "~"))

(use-package dired
  :ensure nil
  :demand t
  :bind (("C-c d" . dired-jump)
	 :map dired-mode-map
	 ("/" . dired-omit-mode)
	 ("H" . dired-hide-details-mode)
	 ("<" . dired-up-directory)
	 ("e" . wdired-change-to-wdired-mode))
  :hook
  (dired-hide-details-mode .
			   dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq delete-by-moving-to-trash t)
  (setq dired-auto-revert-buffer t)
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls" dired-use-ls-dired t)))

(use-package nerd-icons-dired
  :ensure t
  :demand t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; (use-package treemacs-icons-dired
;;   :ensure t
;;   :defer t
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package smartparens-config
  :after smartparens
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "'" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "`" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

(use-package smartparens
  :ensure t
  :hook
  (prog-mode . smartparens-mode)
  (cider-repl-mode . smartparens-mode)
  :config
  (setq sp-show-pair-from-inside t)
  (setq sp-base-key-bindings 'sp))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 1.0)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25))

;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(setq lsp-enable-indentation nil)

(use-package minions
  :ensure t
  :custom
  (minions-mode 1))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t)
  :custom
  (doom-modeline-time-icon nil)
  ;; (doom-modeline-time-clock-size 0.9)
  (doom-modeline-time-live-icon nil)
  ;; (doom-modeline-time-analogue-clock t)
  (doom-modeline-time t)
  ;; (doom-modeline-position-column-line-format "C%c L%l")
  ;; (doom-modeline-position-line-format "L%l")
  (doom-modeline-total-line-number t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-mu4e t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-vcs-max-length 40)
  (doom-modeline-minor-modes t))


(set-face-attribute 'header-line nil
		    :family "Ingra"
		    :weight 'medium
		    :background "#0a070e"
		    ;; :foreground "white"
		    :box '(:line-width (1 . 4) :color "#0a070e")
		    :overline nil
		    :underline nil)

(set-face-attribute 'mode-line nil
		    :family "Ingra"
		    :weight 'medium
                    :background "#252567"
                    :box '(:line-width 3 :color "#252567")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
		    :family "Ingra"
		    :weight 'regular
                    :background "#393965"
                    :box '(:line-width 3 :color "#393965")
                    :overline nil
                    :underline nil)

(set-face-foreground 'line-number "#373041")
(set-face-attribute 'line-number nil :height 0.9)
;;(set-face-attribute 'line-number-current-line nil :height 0.9)
(set-face-foreground 'line-number-current-line "#888888")
(set-face-background 'line-number-current-line nil)
;; right border for current line

(use-package lsp-ui
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :defer t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-enable-symbol-highlighting t)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Show auto completions after 2 characters
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
 
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode 1)
  :config
  (define-key corfu-map (kbd "<escape>") 'corfu-quit))

(global-set-key (kbd "C-x 5 6") 'tear-off-window)

(global-set-key (kbd "M-s-s") 'mark-end-of-sentence)
(global-set-key (kbd "C-c m m") 'minimap-mode)
(global-set-key (kbd "M-o") 'other-window)
;; (define-prefix-command 'jester-map)
;; (global-set-key (kbd "M-s") 'jester-map)
;; (global-set-key (kbd "M-s o") 'occur)
(global-set-key (kbd "s-b b") 'switch-to-buffer)
(global-set-key (kbd "s-b d") 'dired-jump)
(global-set-key (kbd "s-K") 'kill-current-buffer)
(global-set-key (kbd "s-k") 'kill-buffer-and-window)
(global-set-key (kbd "s-b m") 'switch-to-minibuffer)
(global-set-key (kbd "C-c f") 'comint-dynamic-complete-filename)
(global-set-key (kbd "s-r") 'rgrep)

(defun matches-a-buffer-name (name)
  "Return non-nil if NAME matches the name of an existing buffer."
  (try-completion name (mapcar #'buffer-name (buffer-list))))

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-space-between-root-nodes nil)
  :config
  (treemacs-git-commit-diff-mode t)
  (treemacs-follow-mode t)
  (set-face-attribute 'treemacs-root-face nil ; set height 1.0, no underline
		      :family "Ingra"
		      :weight 'regular
		      :height 1.0
		      :underline nil
                      :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'treemacs-file-face nil
		      :family "Ingra"
		      :weight 'regular
		      :height 1.0
		      :underline nil)
  (set-face-attribute 'treemacs-directory-face nil
		      :family "Ingra"
		      :weight 'regular
		      :height 1.0
		      :underline nil)
  (treemacs-define-custom-image-icon
   "~/.emacs.d/jester/treemacs-icons/csharp.png" "cs")
  (defun treemacs-ignore-meta-files (filename absolute-path)
    "Ignore files with .meta extension (Unity files)."
    (string-suffix-p ".meta" filename))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-meta-files)
  (setq treemacs-width-is-initially-locked nil
	treemacs-lock-width nil
	treemacs-width 30
	treemacs-width-increment 1
	treemacs-show-hidden-files t)
  (treemacs-hide-gitignored-files-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
	("s-b t t"   . treemacs)
        ("s-b t d"   . treemacs-select-directory)
        ("s-b t b"   . treemacs-bookmark)
	("s-b t w"   . treemacs-switch-workspace)
        ("s-b t f" . treemacs-find-file)))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;; (load "~/.emacs.d/jester/tabs-config")

;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;(setq org-roam-directory "/Volumes/Notes/org-roam")
;(org-roam-db-autosync-mode)

(use-package typescript-mode
  :ensure t
  :defer t)

(add-hook 'clojure-mode-hook 'lsp)
(setq eglot-completion-function 'corfu-completion-at-point)
(setq eglot-ignored-server-capabilities '(:textDocument/prepareTypeHierarchy))
(setq warning-suppress-types '((emacs)))
(add-hook 'csharp-ts-mode-hook 'lsp) ;; 'eglot-ensure
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
(add-hook 'javascript-mode 'lsp)
(add-hook 'typescript-mode 'lsp)

(use-package hs-minocr-mode
  :hook
  (prog-mode . hs-minor-mode))

;(setq 'imenu-sort-function 'imenu--sort-by-position)
;(setq 'imenu-auto-rescan )

(add-hook 'prog-mode-hook 'imenu-add-menubar-index)





(server-start)

;; https://stackoverflow.com/questions/8993183/emacs-scroll-buffer-not-point
;;;_*======================================================================
;;;_* define a function to scroll with the cursor in place, moving the
;;;_* page instead
;; Navigation Functions
(defun scroll-down-in-place (n)
  "Scroll down N lines, keeping the cursor in place."
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun scroll-up-in-place (n)
  "Scroll up N lines, keeping the cursor in place."
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

;; (global-set-key "\M-n" 'scroll-up-in-place)
;; (global-set-key "\M-p" 'scroll-down-in-place)

(defun scroll-all-windows-up (&optional arg)
  "Scroll all windows in the current frame up by ARG lines."
  (interactive "p")
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (scroll-up arg)))
                nil t))

(defun scroll-all-windows-down (&optional arg)
  "Scroll all windows in the current frame down by ARG lines."
  (interactive "p")
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (scroll-down arg)))
                nil t))

(global-set-key "\M-N" (lambda (arg) (interactive "p") (scroll-other-window arg)))
(global-set-key "\M-P" (lambda (arg) (interactive "p") (scroll-other-window-down arg)))
(global-set-key (kbd "C-M-s-n") 'scroll-all-windows-up)
(global-set-key (kbd "C-M-s-p") 'scroll-all-windows-down)


;; (use-package copilot
;;   :defer t
;;   ;; :quelpa (copilot :fetcher github
;;   ;; 		   :repo "copilot-emacs/copilot.el"
;;   ;; 		   :branch "main"
;;   ;; 		   :files ("*.el"))
;;   :hook
;;   (emacs-lisp-mode . copilot-mode)
;;   (clojure-mode . copilot-mode)
;;   (js-ts-mode . copilot-mode)
;;   (javascript-mode . copilot-mode)
;;   (js-mode . copilot-mode)
;;   (typescript-mode . copilot-mode)
;;   (csharp-mode . copilot-mode)
;;   (csharp-ts-mode . copilot-mode)
;;   (c-mode . copilot-mode)
;;   (java-mode . copilot-mode)
;;   (solidity-mode . copilot-mode)
;;   (web-mode . copilot-mode)
;;   (css-mode . copilot-mode)
;;   :config
;;   (global-set-key (kbd "M-s c") 'copilot-mode)
;;   (define-key copilot-completion-map (kbd "<escape>") 'copilot-clear-overlay)
;;   (define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion-by-word)
;;   (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-line)
;;   (define-key copilot-completion-map (kbd "C-M-<tab>") 'copilot-accept-completion)
;;   (setq copilot-indent-offset-warning-disable t))

(defun my-get-api-key (key)
  (let ((key-file (concat "~/.emacs." key "-key")))
    (with-temp-buffer
      (insert-file-contents-literally key-file)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(load-file "~/.emacs.d/jester/gptel-config.el")
;; (load-file "~/.emacs.d/jester/gptel-copilot.el")
(load-file "~/.emacs.d/jester/llm-chat-search.el")

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-fontify-code-blocks-natively t)
  ;; :bind
  ;; ("C-c x" . jester/toggle-markdown)
  :config
  (define-key markdown-mode-map (kbd "<mouse-1>") 'markdown-toggle-gfm-checkbox)
  (define-key markdown-mode-map (kbd "C-s-t") 'markdown-toggle-gfm-checkbox)
  (setq markdown-max-image-size '(1400 . 600))
  :hook
  (markdown-mode . (lambda ()
		     (push '("[ ]" . "🔳") prettify-symbols-alist)
		     (push '("[x]" . "✅" ) prettify-symbols-alist)
		     (push '("[-]" . "⚪" ) prettify-symbols-alist)
		     (prettify-symbols-mode))))

(load "~/.emacs.d/jester/markdown-edit-on-line-move")

(use-package solidity-mode
  :defer t
  :ensure t
  :config
  (setq solidity-comment-style 'slash)
  (define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point))

(use-package solidity-flycheck
  :defer t
  :ensure t
  :config
  (setq solidity-flycheck-solc-checker-active t)
  (setq flycheck-solidity-solc-addstd-contracts t))

(use-package company-solidity
  :defer t
  :ensure t)

(use-package zig-mode
  :defer t
  :ensure t)

;; add key binding for eww C-c C-w
(global-set-key (kbd "C-c w") 'eww)

(defun browse-hacker-news ()
  "Open hacker news in eww"
  (interactive)
  (eww "https://news.ycombinator.com"))


(defun browse-unity-manual ()
  "Open Unity Manual in xwidget browser"
  (interactive)
  (xwidget-webkit-browse-url "file:///Volumes/Om/Applications_x86/Unity/Hub/Editor/6000.0.5f1/Documentation/en/Manual/index.html"))

;; key binding for hacker news
(global-set-key (kbd "C-c h") 'browse-hacker-news)

;; key binding for unity manual
(global-set-key (kbd "C-c u") 'browse-unity-manual)

;; key binding for kill buffer and window
(global-set-key (kbd "<f4>") 'kill-buffer-and-window)

(defun delete-frame-killing-buffer ()
  "Delete the current frame and kill the buffer."
  (interactive)
  (kill-buffer)
  (delete-frame))

;; (global-set-key (kbd "C-x 4 4") 'delete-frame-killing-buffer)

;; key binding for eval-region
(global-set-key (kbd "C-c e") 'eval-region)



(defun jester/disable-icons ()
  "Disable nerd-icons in dired."
  (nerd-icons-dired-mode 0)
  ;;(treemacs-icons-dired-mode 0)
  )
(defun jester/restore-icons ()
  "Restore nerd-icons in dired."
  (nerd-icons-dired-mode 1)
  ;;(treemacs-icons-dired-mode 1)
  )

(advice-add 'wdired-change-to-wdired-mode
            :before #'jester/disable-icons)
(advice-add 'wdired-finish-edit
            :after #'jester/restore-icons)
(advice-add 'wdired-abort-changes
             :after #'jester/restore-icons)

(defun shake-frame (&optional frame duration displacement)
  "Shakes the specified FRAME or current frame slightly.
If duration is not provided, use 0.34 seconds.
If displacement is not provided, defaults to 10 pixels."
  (interactive)
  (let ((frame (or frame (selected-frame)))
	(frame-pos (frame-position))
        (start-time (current-time))
        (duration (or duration 0.10))  ; shake duration in seconds
        (displacement (or displacement 10)))  ; displacement in pixels
    (catch 'exit
      (while (< (float-time (time-since start-time)) duration)
        (set-frame-position (selected-frame)
                             (+ (car frame-pos) (random displacement))
                             (+ (cdr frame-pos) (random displacement)))
        (redisplay t)
        (sit-for 0.01))
      (set-frame-position (selected-frame) (car frame-pos) (cdr frame-pos)))))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; (load-file "~/.emacs.d/jester/codeium-config.el")
;; (run-with-idle-timer 3 nil
;;                      (lambda ()
;;                        (load-file "~/.emacs.d/jester/codeium-config.el")))


;; (load-file "~/.emacs.d/jester/extra-config.el")
(load "~/.emacs.d/jester/smartscan")
(load-machine-specific-config "mu4e")
(global-set-key (kbd "C-c m x") 'mu4e)
(load-machine-specific-config "org-roam")

;; (defun jester/current-song ()
;;   "Get the current song playing in Apple Music."
;;   (interactive)
;;   ;; Run the command in a temporary buffer and capture the output
;;   (let ((output-buffer (generate-new-buffer "*current-song-temp*")))
;;     (shell-command "shortcuts run current-song && pbpaste" output-buffer)
;;     (with-current-buffer output-buffer
;;       (let ((song-info (buffer-string)))
;;         (message "Current song: %s" song-info)))
;;     ;; Clean up the temporary buffer
;;     (kill-buffer output-buffer)))

;; (defun jester/current-song ()
;;   "Get the current song playing in Apple Music from the file ~/.current-song.txt."
;;   (interactive)
;;   (start-process "current-song-process" nil "shortcuts" "run" "current-song")
;;   (let ((file-path (expand-file-name "~/.current-song.txt")))
;;     (if (file-exists-p file-path)
;;         (with-temp-buffer
;;           ;; Insert the contents of the file into the temporary buffer
;;           (insert-file-contents file-path)
;;           ;; Extract the first line of the buffer
;;           (let ((song-info (buffer-substring-no-properties
;;                             (line-beginning-position)
;;                             (line-end-position))))
;;             (message "Current song: %s" song-info)))
;;       (message "File %s does not exist." file-path))))

;;(require 'cl-lib)
(eval-when-compile (require 'cl))


(defun jester/current-song ()
  "Get the current song playing in Apple Music from the file ~/.current-song.txt."
  (interactive)
  (lexical-let ((file-path (expand-file-name "~/.current-song.txt")))
    ;; Start the async process
    (let ((process (start-process "current-song-process" nil "shortcuts" "run" "current-song")))
      ;; Define the sentinel to handle the process completion
      (set-process-sentinel process
                            (lambda (proc event)
                              ;; Use the captured file-path
                              (when (string= event "finished\n")
                                (if (file-exists-p file-path)
                                    (with-temp-buffer
                                      ;; Insert the contents of the file into the temporary buffer
                                      (insert-file-contents file-path)
                                      ;; Extract the first line of the buffer
                                      (let ((song-info (buffer-substring-no-properties
                                                        (line-beginning-position)
                                                        (line-end-position))))
                                        (message "Now playing: %s" song-info)))
                                  (message "File %s does not exist." file-path))))))))

(defun jester/play-pause ()
  "Play or pause the current song in Apple Music."
  (interactive)
  (let ((process (start-process "play-pause-process" nil "shortcuts" "run" "music-play-pause")))
    ;; Define the sentinel to run when the process finishes
    (set-process-sentinel process
			  (lambda (proc event)
			    (when (string= event "finished\n")
			      ;; Call jester/current-song to display the current song
			      (jester/current-song))))))

(defun jester/next-song ()
  "Play the next song in Apple Music and then display the current song."
  (interactive)
  (let ((process (start-process "next-song-process" nil "shortcuts" "run" "next-song")))
    ;; Define the sentinel to run when the process finishes
    (set-process-sentinel process
                          (lambda (proc event)
                            (when (string= event "finished\n")
                              ;; Call jester/current-song to display the current song
                              (jester/current-song))))))

(defun jester/previous-song ()
  "Play the previous song in Apple Music and then display the current song."
  (interactive)
  (let ((process (start-process "previous-song-process" nil "shortcuts" "run" "previous-song")))
    ;; Define the sentinel to run when the process finishes
    (set-process-sentinel process
                          (lambda (proc event)
                            (when (string= event "finished\n")
                              ;; Call jester/current-song to display the current song
                              (jester/current-song))))))


(global-set-key (kbd "M-s-m c") 'jester/current-song)
(global-set-key (kbd "M-s-m m") 'jester/play-pause)
(global-set-key (kbd "M-s-m n") 'jester/next-song)
(global-set-key (kbd "M-s-m p") 'jester/previous-song)


(setq tramp-use-ssh-controlmaster-options nil)

(load-file "~/.emacs.d/jester/snippets.el")

(when (eq my-machine-type 'mac-laptop) (set-face-attribute 'default nil :height 145))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))


(use-package org-modern
  :ensure t
  :after org
  :defer t
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-checkbox
   '((?X . "⦿")    ;; checked
     (?- . "-")   ;; partial
     (?\s . "⦾"))) ;; empty
  (org-modern-fold-stars
   '(("▶" . "▼") ("▷" . "▽") ("▶" . "▼") ("▹" . "▿") ("▸" . "▾")))
  :config
  (setq org-modern-todo-faces
   '(("TODO" :inherit (org-todo) :family "Ingra")
     ("DONE" :inherit (org-done) :family "Ingra")))
  (setq org-modern-priority-faces
   '((?A :inherit (org-priority) :family "Ingra")
     (?B :inherit (org-priority) :family "Ingra")
     (?C :inherit (org-priority) :family "Ingra"))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(desktop-save-mode 1)
(setq desktop-restore-eager 2)
