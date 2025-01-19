;;; diccionario RAE / DRAE / DLE

(defun dle-buscar-palabra (word)
  "Busca PALABRA en el diccionario DLE utilizando buscar-dle.sh.
Si se invoca interactivamente sin una región activa, solicita la palabra.
Si hay una región activa, utiliza el texto de la región como la palabra."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Palabra a buscar en el Diccionario de la lengua española: "))))
  
  ;; Trim whitespace and ensure word is not empty
  (setq word (string-trim word))
  (when (string-empty-p word)
    (user-error "No se proporcionó una palabra."))
  
  ;; Create or get buffer for results
  (let ((buffer-name "*DLE Definitions*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
        (insert "\n\n------------------\n")
        ;; Insert the word that was looked up
        (insert (concat word "\n"))
        ;; Run the shell command and insert its output
        (call-process "buscar-dle.sh" nil t nil word)
        ;; Make buffer read-only
        (special-mode)
        ;; Switch to the buffer to recenter
        (switch-to-buffer buffer-name)
        (recenter)))))

(defun dle-insertar-definicion ()
  "Busca la palabra en el punto en el diccionario DLE e inserta la definición después de la palabra."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (let ((output (shell-command-to-string (concat "buscar-dle.sh " word))))
        (insert "\n" output)))))

(global-set-key (kbd "C-c B") 'dle-buscar-palabra)
(global-set-key (kbd "C-c D") 'dle-insertar-definicion)

;; Local Variables:
;; jinx-languages: "es en"
;; End:
