(require 'gptel) ; makes gptel--openai-models available in use-package

(use-package gptel
  :defer nil
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-use-tools t)
  (setq gptel-track-media t)
  (setq gptel-backend (gptel-make-openai "openai-with-parallel-tool-calls"
			:key (my-get-api-key "openai")
			:stream t
			:models gptel--openai-models
			:request-params '(:parallel_tool_calls t))))

(setq gptel-tools
      (list
       ;; Echo to scratch tool
       (gptel-make-tool
        :function (lambda (text)
                    (with-current-buffer "*scratch*"
                      (goto-char (point-max))
                      (insert (format "%s\n" text)))
                    (format "Appended to scratch: %s" text))
        :name "echo_scratch"
        :description "Append a message to the *scratch* buffer"
        :args (list '(:name "text"
                     :type "string"
                     :description "The text to append to the scratch buffer")))
       
       ;; Message buffer logging tool
       (gptel-make-tool
        :function (lambda (text)
                    (message "%s" text)
                    (format "Message sent: %s" text))
        :name "echo_message"
        :description "Send a message to the *Messages* buffer"
        :args (list '(:name "text"
                     :type "string"
                     :description "The text to send to the messages buffer")))
       
       ;; Scratch buffer retrieval tool
       (gptel-make-tool
        :function (lambda ()
                    (with-current-buffer "*scratch*"
                      (buffer-substring-no-properties (point-min) (point-max))))
        :name "get_scratch_buffer"
        :description "Return the contents of the *scratch* buffer"
        :args nil)))

