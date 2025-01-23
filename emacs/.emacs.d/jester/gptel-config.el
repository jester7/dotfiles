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
    (setq gptel-backend-openai (gptel-make-openai "OpenAI"
                                 :host "api.openai.com"
                                 :key (my-get-api-key "openai")
                                 :stream t
                                 :models '("o1-preview" "o1-mini" "gpt-4o-mini" "gpt-4o")))
    
    (setq gptel-backend-claude (gptel-make-anthropic "Claude"
                                 :key (my-get-api-key "claude")
                                 :stream t
                                 :models '("claude-3-5-sonnet-20241022"
					   "claude-3-5-haiku-20241022"
					   "claude-3-opus-20240229")))
    
    (setq gptel-backend-gemini (gptel-make-gemini "Gemini"
				 :key (my-get-api-key "gemini")
				 :stream t
				 :models '("gemini-1.5-pro"
					   "gemini-1.5-flash"
					   "gemini-1.5-flash-8b"
					   "gemini-exp-1206"
					   "gemini-2.0-flash-thinking-exp-1219"
					   "gemini-2.0-flash-exp")))
    
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
					      "deepseek-ai/DeepSeek-R1-Distill-Llama-70B"
					      "Qwen/QwQ-32B-Preview"
					      "Qwen/QVQ-72B-Preview"
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
      (setq gptel-backend gptel-backend-claude)
      (setq gptel-model (car (gptel-backend-models gptel-backend)))))

  (defun gptel-set-model (model-spec)
    "Set the gptel model interactively."
    (interactive
     (list (completing-read "Choose model (backend:model): "
                            (cl-loop for backend-sym in '(gptel-backend-openai
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
       ;; URL reading tool
       (gptel-make-tool
	:function (lambda (url)
		    (with-current-buffer (url-retrieve-synchronously url)
		      (goto-char (point-min))
		      (re-search-forward "\n\n")
		      (buffer-substring-no-properties (point) (point-max))))
	:name "read_url"
	:description "Fetch and read the contents of a URL"
	:args (list '(:name "url"
			    :type "string"
			    :description "The URL to read")))

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
        :args nil)
       

       (gptel-make-tool
	:function (lambda (directory)
		    (shell-command-to-string 
		     (format "ls %s" 
			     (shell-quote-argument 
			      (expand-file-name directory)))))
	:name "list_directory"
	:description "List the contents of a given directory"
	:args (list '(:name "directory"
			    :type "string"
			    :description "The path to the directory to list")))
       
       (gptel-make-tool
        :function (lambda (parent name)
                    (condition-case nil
                        (progn
                          (make-directory (expand-file-name name parent) t)
                          (format "Directory %s created/verified in %s" name parent))
                      (error (format "Error creating directory %s in %s" name parent))))
        :name "make_directory"
        :description "Create a new directory with the given name in the specified parent directory"
        :args (list '(:name "parent"
			    :type "string"
			    :description "The parent directory where the new directory should be created")
                    '(:name "name"
			    :type "string"
			    :description "The name of the new directory to create")))

       (gptel-make-tool
        :function (lambda (path filename content)
                    (let ((full-path (expand-file-name filename path)))
                      (with-temp-buffer
                        (insert content)
                        (write-file full-path))
                      (format "Created file %s in %s" filename path)))
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
			    :description "The content to write to the file")))

       (gptel-make-tool
	:function (lambda (filepath)
		    (with-temp-buffer
		      (insert-file-contents (expand-file-name filepath))
		      (buffer-string)))
	:name "read_file"
	:description "Read and display the contents of a file"
	:args (list '(:name "filepath"
			    :type "string"
			    :description "Path to the file to read (supports relative paths and ~)")))

       (gptel-make-tool
        :function (lambda (directory)
                    (shell-command-to-string 
                     (format "bb %s %s" 
                             (expand-file-name "ls-tree-view.bb" "~/bin")
                             (shell-quote-argument directory))))
        :name "list_directory_tree_view"
        :description "Show a tree view of the specified directory"
        :args (list '(:name "directory"
			    :type "string"
			    :description "The directory to show the tree view for")))))
