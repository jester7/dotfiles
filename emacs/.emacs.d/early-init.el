;;(setq load-suffixes '(".el"))
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq load-prefer-newer t)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 120))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setq read-process-output-max (* 1024 1024))

(when (display-graphic-p)
  (if (member "Darkmode Mono On" (font-family-list))
      (set-face-attribute 'default nil
                          :family "Darkmode Mono On"
                          :weight 'medium
                          :slant 'normal
                          :height 145)
    (set-face-attribute 'default nil
			:family "MesloLGS NF"
			:weight 'regular
			:slant 'normal
                        :height 145)))

(setq native-comp-async-report-warnings-errors nil)

(defconst my-machine-type
  (cond
   ((string-equal (system-name) "Terra.local") 'mac-desktop)
   ((string-equal (system-name) "Janus.local") 'mac-laptop)
   ((eq system-type 'gnu/linux) 'linux-server)
   (t 'unknown)))

(defun load-machine-specific-config (config-name)
  "Load a machine-specific configuration file based on
   CONFIG-NAME and `my-machine-type`."
  (let* ((config-dir (expand-file-name "~/.emacs.d/jester/"))
         (file (concat config-dir config-name "-" (symbol-name my-machine-type) ".el")))
    (if (file-exists-p file)
        (load file)
      (message "Machine-specific config %s not found" file))))

(let ((base-threshold
       (cond
        ((eq my-machine-type 'mac-desktop) (* 1024 1024 1024))
        ((eq my-machine-type 'mac-laptop) (* 1024 1024 256))
        ((eq my-machine-type 'linux-server) (* 1024 1024 64))
        (t (* 1024 1024 128))))) ; Default 128MB for unknown types
  (setq gc-cons-threshold base-threshold
        gc-cons-percentage 0.6))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold
                  (cond
                   ((eq my-machine-type 'mac-desktop) (* 64 1024 1024))
                   ((eq my-machine-type 'mac-laptop)  (* 20 1024 1024))
                   ((eq my-machine-type 'linux-server) (* 10 1024 1024))
                   (t (* 20 1024 1024))) ; Default 20 MB for unknown types
                  gc-cons-percentage 0.3)))


(defun get-latest-version-in-dir (dir)
  "Get latest version in directory assuming semantic versioning."
  (when (file-directory-p dir)
    (car (sort (directory-files dir nil "^v?[0-9]" t) #'string>))))


(defun get-latest-node-path ()
  "Get path to latest node version."
  (let* ((node-versions-dir "/Users/jester/.nvm/versions/node")
         (latest-version (get-latest-version-in-dir node-versions-dir)))
    (when latest-version
      (concat node-versions-dir "/" latest-version "/bin"))))

(defun get-latest-dotnet-path ()
  "Get path to latest dotnet SDK."
  (let* ((dotnet-dir "/usr/local/Cellar/dotnet")
         (latest-version (get-latest-version-in-dir dotnet-dir))
         (sdk-dir (concat dotnet-dir "/" latest-version "/libexec/sdk"))
         (latest-sdk (get-latest-version-in-dir sdk-dir)))
    (when (and latest-version latest-sdk)
      (concat sdk-dir "/" latest-sdk "/Sdks"))))

(let ((new-paths (delq nil
                       (list
			(get-latest-node-path)
			"/Users/jester/bin"
			"/usr/local/opt/openjdk/bin"
			"/usr/local/bin"
			"/usr/local/sbin"
			"/System/Cryptexes/App/usr/bin"
			"/usr/bin"
			"/bin"
			"/usr/sbin"
			"/sbin"
			"/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
			"/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
			"/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
			"/usr/local/share/dotnet"
			"/Users/jester/.cargo/bin"
			"/Users/jester/.dotnet/tools"
			(get-latest-dotnet-path)
			"/opt/homebrew/bin"))))
  (setq exec-path (append new-paths exec-path))
  (setenv "PATH" (string-join exec-path ":")))
