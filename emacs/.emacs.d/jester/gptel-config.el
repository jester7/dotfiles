(use-package gptel
  :defer nil
  :ensure t
  :custom
  ;; use whole doc by default
  (gptel-org-branching-context nil)
  (gptel-default-mode 'org-mode)
  :init
  (require 'cl-lib)
  (defun gptel-init-backends ()
    "Initialize GPTel backends."
    (setq gptel-backend-xai (gptel-make-xai "xAI"
			      :key (my-get-api-key "xai")
			      :stream t
			      :models '("grok-4-fast-reasoning"
					"grok-4-fast-non-reasoning"
					"grok-code-fast-1"
					"grok-4"
					"grok-3-mini")))
    (setq gptel-backend-openai (gptel-make-openai "OpenAI"
                                 :host "api.openai.com"
                                 :key (my-get-api-key "openai")
                                 :stream t
                                 :models '("gpt-5"
					   "gpt-5-mini"
					   "gpt-5-nano"
					   "gpt-4.1")))
    
    (setq gptel-backend-claude (gptel-make-anthropic "Claude"
                                 :key (my-get-api-key "claude")
                                 :stream t
                                 :models '("claude-sonnet-4-5-20250929"
					   "claude-haiku-4-5-20251001"
					   "claude-opus-4-1-20250805")))
    
    (setq gptel-backend-gemini (gptel-make-gemini "Gemini"
				 :key (my-get-api-key "gemini")
				 :stream t
				 :models '("gemini-2.5-pro"
					   "gemini-flash-latest"
					   "gemini-flash-lite-latest"
					   "gemini-2.0-flash-lite")))
    
    (setq gptel-backend-deepinfra (gptel-make-openai "deepinfra"
				    :host "api.deepinfra.com"
				    :endpoint "/v1/openai/chat/completions"
				    :key (my-get-api-key "deepinfra")
				    :stream t
				    :models '("meta-llama/Llama-3.3-70B-Instruct"
					      "meta-llama/Llama-3.3-70B-Instruct-Turbo"
					      "meta-llama/Meta-Llama-3.1-8B-Instruct-Turbo"
					      "meta-llama/Meta-Llama-3.1-405B-Instruct"
					      "deepseek-ai/DeepSeek-V3"
					      "deepseek-ai/DeepSeek-R1"
					      "deepseek-ai/DeepSeek-R1-Turbo"
					      "deepseek-ai/DeepSeek-R1-Distill-Llama-70B"
					      "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B"
					      "Qwen/Qwen3-Next-80B-A3B-Instruct"
					      "Qwen/Qwen2.5-72B-Instruct"
					      "Qwen/Qwen2.5-Coder-32B-Instruct")))
    
    (setq gptel-backend-together (gptel-make-openai "together.ai"
				   :host "api.together.xyz"
				   :key (my-get-api-key "together")
				   :stream t
				   :models '("meta-llama/Llama-3.3-70B-Instruct-Turbo-Free"
					     "deepseek-ai/DeepSeek-V3"
					     "deepseek-ai/DeepSeek-R1"
					     "codellama/CodeLlama-34b-Instruct-hf"))))

  (defun gptel-ensure-backends ()
    "Ensure GPTel backends are initialized and set a default backend."
    (unless (bound-and-true-p gptel-backend-openai)
      (gptel-init-backends)
      (setq gptel-backend gptel-backend-gemini)
      (setq gptel-model (car (gptel-backend-models gptel-backend)))))

  (defun gptel-set-model (model-spec)
    "Set the gptel model interactively."
    (interactive
     (list (completing-read "Choose model (backend:model): "
                            (cl-loop for backend-sym in '(gptel-backend-xai
							  gptel-backend-openai
							  gptel-backend-claude
							  gptel-backend-gemini
							  gptel-backend-deepinfra
							  gptel-backend-together)
                                     for backend = (symbol-value backend-sym)
                                     append (cl-loop for model in (gptel-backend-models backend)
                                                     collect (format "%s:%s" (symbol-name backend-sym) model))))))

    (let* ((parts (split-string model-spec ":"))
           (backend-sym (intern (car parts)))
           (model-name (cadr parts))
           (backend (symbol-value backend-sym)))

      (unless (and backend (boundp backend-sym))
	(error "Unknown or unbound backend: %s" (car parts)))

      ;; Correctly check for model availability using 'mapcar' and 'string='
      (unless (cl-some (lambda (model) (string= model-name model)) (gptel-backend-models backend))
	(error "Model %s not available for backend %s" model-name (symbol-name backend-sym)))

      (setq gptel-backend backend)
      (setq gptel-model (intern model-name))
      (message "GPTel model set to: %s:%s" (symbol-name backend-sym) model-name)))
  
  :config  
  (setq gptel-use-tools t)
  (setq gptel-track-media t)
  (gptel-ensure-backends)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  (progn (declare-function org-element-lineage-map "org-element-ast")
         (defalias 'gptel-org--element-lineage-map 'org-element-lineage-map))


  (defun gptel-org-toggle-branching-context ()
    "Toggle gptel context between doc and subheading."
    (interactive)
    (if gptel-org-branching-context
        (progn
          (setq-local gptel-org-branching-context nil)
          (message "Context: whole doc"))
      (setq-local gptel-org-branching-context t)
      (message "Context: subheading")))
  
  (add-hook 'gptel-mode-hook 'gptel-ensure-backends)

  (global-set-key (kbd "C-c g g") 'gptel)
  (global-set-key (kbd "C-c g M") 'gptel-set-model)
  (global-set-key (kbd "C-c g m") 'gptel-menu)
  (global-set-key (kbd "C-c g a") 'gptel-add)
  (global-set-key (kbd "C-c g b") 'gptel-org-toggle-branching-context)
  ;; (global-set-key (kbd "s-<return>") 'gptel-send)

  (setq gptel-directives
	'((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (thorough . "You are a large language model living in Emacs and a helpful assistant. Respond thoroughly, addressing all questions and concerns fully.")
          (code-completion . "Complete the following code in a concise and idiomatic style, considering the surrounding context. Provide ONLY the code necessary to complete the current statement or expression. Do NOT include any explanations, notes, or introductory phrases. Output MUST be plain text code without any markdown formatting or surrounding backticks. Generate exactly 3 distinct and valid code completion suggestions, each on a new line without any blank lines or other separators between them. Do NOT insert newlines within each completion suggestion.

Code to complete:")
	  
          (programming-es . "Eres un asistente de programación integrado directamente en Emacs. Responde con código en el lenguaje de programación apropiado, usando español para todos los comentarios.")
          (writing-completion . "You are a large language model living in Emacs and an expert writing assistant.
Respond only with enough text to complete the current sentence or paragraph in a style that mimics the provided text.")
          (writing-editor . "You are a large language model living in Emacs and an expert writing assistant. Improve the provided text, correcting any spelling, grammar, and style issues.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (default-es . "Eres un modelo de lenguaje de gran tamaño que vive en Emacs y un asistente útil. Responde de manera concisa.")
          (thorough-es . "Eres un modelo de lenguaje de gran tamaño que vive en Emacs y un asistente útil. Responde a fondo, abordando todas las preguntas e inquietudes en su totalidad.")
          (writing-completion-es . "Eres un modelo de lenguaje de gran tamaño que vive en Emacs y un asistente de escritura y redacción experto.
Responde sólo con el texto suficiente para completar la oración o el párrafo actual en un estilo que imite el texto proporcionado.")
          (writing-editor-es . "Eres un modelo de lenguaje de gran tamaño que vive en Emacs y un asistente experto de escritura y redacción. Mejora el texto proporcionado, corrigiendo cualquier error ortográfico, gramatical y de estilo."))))



(defun gptel-copilot-complete-with-minibuffer ()
  "Query GPTel for completions and display them with 'completing-read'."
  (interactive)
  (unless (and gptel-backend gptel-model)
    (error "GPTel backend and model must be configured"))
  (let ((gptel--num-messages-to-send 0)
        (prompt (or (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                    (and (> (point) (point-min)) (buffer-substring-no-properties (point-at-bol) (point)))
                    "")))
    (gptel-request prompt
      :callback (lambda (response _info)
                  (when (and response (stringp response))
                    (let* ((completions (split-string response "\n" t))
                           (selected (completing-read "GPTel Completion: " completions)))
                      (when (and selected (not (string-empty-p selected)))
                        (insert selected))))))))


(setq gptel-tools
      (list
       ;; ===== EMACS/BUFFER TOOLS =====
       
       ;; Read buffer contents
       (gptel-make-tool
        :function (lambda (buffer)
                    (condition-case err
                        (if (buffer-live-p (get-buffer buffer))
                            (with-current-buffer buffer
                              (buffer-substring-no-properties (point-min) (point-max)))
                          (format "Error: buffer %s is not live." buffer))
                      (error (format "Error reading buffer %s: %s" 
                                     buffer (error-message-string err)))))
        :name "read_buffer"
        :description "Return the contents of an Emacs buffer"
        :args (list '(:name "buffer"
                            :type "string"
                            :description "The name of the buffer to read"))
        :category "emacs")

       ;; Replace entire buffer contents
       (gptel-make-tool
        :function (lambda (buffer-name content)
                    (condition-case err
                        (if (get-buffer buffer-name)
                            (with-current-buffer buffer-name
                              (erase-buffer)
                              (insert content)
                              (format "Buffer contents replaced: %s" buffer-name))
                          (format "Error: Buffer '%s' not found" buffer-name))
                      (error (format "Error replacing buffer %s: %s"
                                     buffer-name (error-message-string err)))))
        :name "replace_buffer"
        :description "Completely replace all contents of a buffer with new content"
        :args (list '(:name "buffer-name"
                            :type "string"
                            :description "The name of the buffer to replace")
                    '(:name "content"
                            :type "string"
                            :description "The new content for the buffer"))
        :category "emacs")

       ;; Append to buffer
       (gptel-make-tool
        :function (lambda (buffer text)
                    (condition-case err
                        (if (buffer-live-p (get-buffer buffer))
                            (with-current-buffer buffer
                              (goto-char (point-max))
                              (insert text)
                              (format "Successfully appended text to buffer %s." buffer))
                          (format "Error: buffer %s is not live or does not exist." buffer))
                      (error (format "Error appending to buffer %s: %s"
                                     buffer (error-message-string err)))))
        :name "append_to_buffer"
        :description "Append text to the end of an Emacs buffer"
        :args (list '(:name "buffer"
                            :type "string"
                            :description "The name of the buffer to append to")
                    '(:name "text"
                            :type "string"
                            :description "The text to append"))
        :category "emacs")

       ;; Search and replace in buffer
       (gptel-make-tool
        :function (lambda (buffer-name search-pattern replacement)
                    (condition-case err
                        (with-current-buffer (get-buffer buffer-name)
                          (save-excursion
                            (goto-char (point-min))
                            (let ((count 0))
                              (while (re-search-forward search-pattern nil t)
                                (replace-match replacement t)
                                (setq count (1+ count)))
                              (format "Replaced %d occurrences in %s" count buffer-name))))
                      (error (format "Error in search/replace: %s" 
                                     (error-message-string err)))))
        :name "buffer_search_replace"
        :description "Perform regex search and replace in a buffer"
        :args (list '(:name "buffer-name" :type "string" 
                            :description "Name of the buffer")
                    '(:name "search-pattern" :type "string" 
                            :description "Regex pattern to search for")
                    '(:name "replacement" :type "string" 
                            :description "Replacement text"))
        :category "emacs")

       ;; Get buffer lines
       (gptel-make-tool
        :function (lambda (buffer-name start-line end-line)
                    (condition-case err
                        (with-current-buffer (get-buffer buffer-name)
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- start-line))
                            (let ((start (point)))
                              (forward-line (- end-line start-line -1))
                              (buffer-substring-no-properties start (point)))))
                      (error (format "Error getting lines: %s" 
                                     (error-message-string err)))))
        :name "get_buffer_lines"
        :description "Extract specific line range from a buffer"
        :args (list '(:name "buffer-name" :type "string" :description "Buffer name")
                    '(:name "start-line" :type "number" :description "Starting line number")
                    '(:name "end-line" :type "number" :description "Ending line number"))
        :category "emacs")

       ;; Buffer info
       (gptel-make-tool
        :function (lambda (buffer-name)
                    (condition-case err
                        (with-current-buffer (get-buffer buffer-name)
                          (format "Mode: %s | Size: %d bytes | Modified: %s | File: %s"
                                  (symbol-name major-mode)
                                  (buffer-size)
                                  (if (buffer-modified-p) "yes" "no")
                                  (or buffer-file-name "none")))
                      (error (format "Error getting buffer info: %s" 
                                     (error-message-string err)))))
        :name "buffer_info"
        :description "Get information about a buffer's state"
        :args (list '(:name "buffer-name" :type "string" :description "Buffer name"))
        :category "emacs")

       ;; Read documentation
       (gptel-make-tool
        :function (lambda (symbol)
                    (condition-case err
                        (let ((sym (intern symbol)))
                          (cond
                           ((fboundp sym)
                            (documentation sym))
                           ((boundp sym)
                            (documentation-property sym 'variable-documentation))
                           (t
                            (format "No documentation found for %s" symbol))))
                      (error (format "Error reading documentation for %s: %s" 
                                     symbol (error-message-string err)))))
        :name "read_documentation"
        :description "Read the documentation for a given function or variable"
        :args (list '(:name "name"
                            :type "string"
                            :description "The name of the function or variable"))
        :category "emacs")

       ;; ===== FILESYSTEM TOOLS =====
       
       ;; Read file
       (gptel-make-tool
        :function (lambda (filepath)
                    (condition-case err
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name filepath))
                          (buffer-string))
                      (error (format "Error reading file %s: %s"
                                     filepath (error-message-string err)))))
        :name "read_file"
        :description "Read and return the contents of a file"
        :args (list '(:name "filepath"
                            :type "string"
                            :description "Path to the file to read"))
        :category "filesystem")

       ;; Create file
       (gptel-make-tool
        :function (lambda (path filename content)
                    (condition-case err
                        (let ((full-path (expand-file-name filename path)))
                          (with-temp-buffer
                            (insert content)
                            (write-file full-path))
                          (format "Created file %s in %s" filename path))
                      (error (format "Error creating file %s in %s: %s" 
                                     filename path (error-message-string err)))))
        :name "create_file"
        :description "Create a new file with the specified content"
        :args (list '(:name "path"
                            :type "string"
                            :description "The directory where to create the file")
                    '(:name "filename"
                            :type "string"
                            :description "The name of the file to create")
                    '(:name "content"
                            :type "string"
                            :description "The content to write to the file"))
        :category "filesystem")

       ;; Overwrite existing file
       (gptel-make-tool
        :function (lambda (filepath content)
                    (let ((filepath (expand-file-name filepath)))
                      (condition-case err
                          (if (file-exists-p filepath)
                              (progn
                                (with-temp-buffer
                                  (insert content)
                                  (write-file filepath))
                                (format "Successfully overwrote existing file %s" filepath))
                            (format "Error: File %s does not exist. Use create_file tool instead." filepath))
                        (error (format "Error overwriting file %s: %s"
                                       filepath (error-message-string err))))))
        :name "overwrite_file"
        :description "Overwrite an existing file with new content (file must already exist)"
        :args (list '(:name "filepath"
                            :type "string"
                            :description "Path to the existing file to overwrite")
                    '(:name "content"
                            :type "string"
                            :description "The new content to write to the file"))
        :category "filesystem")

       ;; Apply diff to file
       (gptel-make-tool
        :function (lambda (filepath diff)
                    (let* ((filepath (expand-file-name filepath))
                           (temp-diff-file (make-temp-file "gptel-diff-"))
                           (result-buffer (get-buffer-create "*gptel-patch-output*")))
                      (unwind-protect
                          (progn
                            (with-temp-file temp-diff-file
                              (insert diff))
                            (let ((status (call-process "patch" nil result-buffer t 
                                                       filepath "-i" temp-diff-file)))
                              (with-current-buffer result-buffer
                                (let ((output (buffer-string))
                                      (success (zerop status)))
                                  (if success
                                      (format "Successfully applied diff to %s" filepath)
                                    (format "Failed to apply diff to %s: %s" filepath output))))))
                        (delete-file temp-diff-file)
                        (kill-buffer result-buffer))))
        :name "apply_diff"
        :description "Apply a unified diff to an existing file"
        :args (list '(:name "filepath"
                            :type "string"
                            :description "Path to the file to update")
                    '(:name "diff"
                            :type "string"
                            :description "The unified diff content to apply"))
        :category "filesystem")

       ;; Make directory
       (gptel-make-tool
        :function (lambda (parent name)
                    (condition-case err
                        (progn
                          (make-directory (expand-file-name name parent) t)
                          (format "Directory %s created/verified in %s" name parent))
                      (error (format "Error creating directory %s in %s: %s" 
                                     name parent (error-message-string err)))))
        :name "make_directory"
        :description "Create a new directory"
        :args (list '(:name "parent"
                            :type "string"
                            :description "The parent directory path")
                    '(:name "name"
                            :type "string"
                            :description "The name of the new directory"))
        :category "filesystem")

       ;; List directory
       (gptel-make-tool
        :function (lambda (directory)
                    (condition-case err
                        (shell-command-to-string 
                         (format "ls -la %s" 
                                 (shell-quote-argument 
                                  (expand-file-name directory))))
                      (error (format "Error listing directory %s: %s"
                                     directory (error-message-string err)))))
        :name "list_directory"
        :description "List the contents of a directory"
        :args (list '(:name "directory"
                            :type "string"
                            :description "The path to the directory to list"))
        :category "filesystem")

       ;; Tree view (your babashka script)
       (gptel-make-tool
        :function (lambda (directory)
                    (condition-case err
                        (shell-command-to-string 
                         (format "bb %s %s" 
                                 (expand-file-name "ls-tree-view.bb" "~/bin")
                                 (shell-quote-argument directory)))
                      (error (format "Error generating tree view: %s"
                                     (error-message-string err)))))
        :name "tree_view"
        :description "Show a tree view of the specified directory using custom babashka script"
        :args (list '(:name "directory"
                            :type "string"
                            :description "The directory to show the tree view for"))
        :category "filesystem")

       ;; ===== COMMAND EXECUTION =====
       
       ;; Run shell command
       (gptel-make-tool
        :function (lambda (command &optional working-dir)
                    (let ((default-directory (if (and working-dir 
                                                      (not (string= working-dir "")))
                                                 (expand-file-name working-dir)
                                               default-directory)))
                      (condition-case err
                          (shell-command-to-string command)
                        (error (format "Command failed: %s" 
                                       (error-message-string err))))))
        :name "run_command"
        :description "Execute a shell command and return its output"
        :args (list '(:name "command"
                            :type "string"
                            :description "The shell command to execute")
                    '(:name "working-dir"
                            :type "string"
                            :description "Optional: directory to run the command in"))
        :category "command")

       ;; ===== WEB TOOLS =====
       
       ;; Fetch URL content
       (gptel-make-tool
	:function (lambda (url)
		    (condition-case err
			(let ((buffer (url-retrieve-synchronously url t nil 10))) ; t=silent, nil=no-cookies, 10=timeout
			  (if buffer
			      (unwind-protect
				  (with-current-buffer buffer
				    (goto-char (point-min))
				    (if (re-search-forward "\n\n" nil t)
					;; SUCCESS: Return a JSON object as an alist
					(list (cons "content" (buffer-substring-no-properties (point) (point-max))))
				      ;; ERROR: Return JSON object as alist
				      (list (cons "error" (format "No HTTP headers found in buffer. Content starts: %s"
								  (buffer-substring-no-properties (point-min) (min (point-max) 100)))))))
				(kill-buffer buffer))
			    ;; ERROR: Return JSON object as alist
			    (list (cons "error" "url-retrieve-synchronously failed to return a buffer."))))
		      ;; CATCHALL ERROR: Return JSON object as alist
		      (error (list (cons "error" (format "Error during URL fetch: %s"
							 (error-message-string err)))))))
	:name "fetch_url"
	:description "Fetch and return the raw content of a URL"
	:args (list '(:name "url"
			    :type "string"
			    :description "The URL to fetch"))
	:category "web")))
