(defvar-local gptel-copilot--completion-start-point nil
  "Buffer-local variable to store the starting point of the completion.")

(defun gptel-copilot--get-completions (prompt)
  "Queries GPTel for completions for the given PROMPT.
   Returns a list of completions or nil if no completions are found."
  (let* ((response-received nil)
         (response-value nil)
         (callback (lambda (response info)
                     (setq response-received t)
                     (setq response-value response))))

    ;; Make the gptel-request call with the callback
    (gptel-request prompt :callback callback)

    ;; Wait for the response to be received
    (while (not response-received)
      (accept-process-output nil 0.1)) ; Check every 100ms

    (when (and response-value (stringp response-value))
      (let ((completions (split-string response-value "\n" t)))
        (if (seq-empty-p completions)
            (message "No completions found.")
          completions)))))

(defun gptel-copilot--completion-function ()
  "Custom completion function for gptel using Corfu."
  (list gptel-copilot--completion-start-point (point)
        (gptel-copilot--get-completions
         (buffer-substring-no-properties (point-at-bol) (point)))))

(defun gptel-copilot-complete-with-corfu ()
  "Query GPTel for completions and display them with Corfu."
  (interactive)
  (unless (and gptel-backend gptel-model)
    (error "GPTel backend and model must be configured."))
  (setq-local gptel-copilot--completion-start-point (save-excursion (goto-char (point-at-bol)) (point)))
  (let ((completion-at-point-functions
         (list #'gptel-copilot--completion-function)))
    (corfu-mode 1)
    (call-interactively #'completion-at-point)))

;; (defun gptel-copilot--corfu-setup ()
;;     (interactive)
;;     ;; configure corfu
;;     (setq corfu-auto t)
;;     (setq corfu-cycle t)
;;     (setq corfu-quit-no-match 'original)
;;     (setq corfu-preview-current nil)
;;     (setq corfu-on-exact-match nil)
;;     (setq corfu-preselect nil)
;;   ;; setup corfu to call gptel-copilot--insert-completion on select
;;     (corfu-mode 1)
;;     )

;; Example usage: bind a key to run gptel completions
(global-set-key (kbd "s-<return>") #'gptel-copilot-complete-with-corfu)
;; (global-set-key (kbd "C-c g s") #'gptel-copilot--corfu-setup)
