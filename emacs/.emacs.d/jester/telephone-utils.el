;;; telephone-utils.el --- Telephone number utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides functions for working with telephone numbers,
;; including opening WhatsApp chats and making phone calls.
;; It prioritizes getting phone numbers from text at point, then the active region,
;; and falls back to prompting the user if neither is available.

;;; Customization variables

(defcustom default-country-code "52"
  "Default country code for phone numbers (e.g., \"52\" for Mexico).
Used for WhatsApp and phone calls when a local number (10 digits or less) is detected."
  :type 'string
  :group 'telephone-utils)

;;; Utility Functions

(defun telephone-utils-clean-phone-number (phone-number)
  "Clean a phone number by removing non-digit characters."
  (replace-regexp-in-string "[^0-9+]+" "" phone-number)) ;; Keep '+' now for international numbers

(defun telephone-utils-get-number-at-point ()
  "Attempt to extract a phone number from the word at point.
Returns the cleaned number string or nil if none found."
  (let ((thing-at-point (thing-at-point 'word)))
    (when thing-at-point
      (let ((cleaned-number (telephone-utils-clean-phone-number thing-at-point)))
        (unless (string-empty-p cleaned-number) ;; Check if cleaning resulted in an empty string
          cleaned-number)))) )


;;; Commands

(defun call-telephone-number ()
  "Make a phone call using system's default handler.
Prioritizes phone number at point, then region. If neither, prompts for input.
For numbers with 10 digits or less, automatically prepends default country code.
For numbers with 11+ digits, ensures they have a '+' prefix if default country code is used.
Country code is customizable via `default-country-code`."
  (interactive)
  (let* ((phone-number
          (or (telephone-utils-get-number-at-point)
              (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
              (read-string "Enter phone number to call: ")))
         (cleaned-number (telephone-utils-clean-phone-number phone-number))
         (formatted-number
          (cond
           ;; Local numbers (10 or fewer digits) - prepend default country code
           ((<= (length cleaned-number) 10)
            (concat "+" default-country-code cleaned-number)) ;; Add '+' for calls
           ;; Longer numbers (11+ digits) - ensure '+' prefix if default country code is used
           ((and (>= (length cleaned-number) 11)
                 (not (string-prefix-p "+" cleaned-number)))
            (concat "+" cleaned-number))
           ;; Otherwise, use the cleaned number as is
           (t
            cleaned-number)))
         (tel-url
          (concat "tel:" formatted-number)))
    (when phone-number ;; Check if we actually got a number before proceeding
      (message "Calling number: %s" formatted-number)
      (browse-url tel-url))))


(defun open-whatsapp-chat ()
  "Open WhatsApp web chat with phone number from point, region, or prompt.
Prioritizes phone number at point, then region. If neither, prompts for input.
For numbers with 10 digits or less, automatically prepends default country code.
Country code is customizable via `default-country-code`."
  (interactive)
  (let* ((phone-number
          (or (telephone-utils-get-number-at-point)
              (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
              (read-string "Enter WhatsApp number: ")))
         (cleaned-number (telephone-utils-clean-phone-number phone-number))
         (formatted-number
          (if (<= (length cleaned-number) 10)
              (concat default-country-code cleaned-number)
            cleaned-number))
         (whatsapp-url
          (concat "https://api.whatsapp.com/send?phone=" formatted-number)))
    (when phone-number ;; Check if we actually got a number before proceeding
      (message "Opening WhatsApp chat with number: %s" formatted-number)
      (browse-url whatsapp-url))))


;;; TODO - work in progress, string quoting/escaping issues WIP
(defun send-sms-message ()
  "Send an SMS message using the macOS Messages app.
Prioritizes phone number at point, then region. If neither, prompts for input.
For numbers with 10 digits or less, automatically prepends default country code and '+'.
For numbers with 11+ digits, ensures they have a '+' prefix if default country code is used.
Uses macOS 'Messages' app to send the SMS."
  (interactive)
  (when (eq system-type 'darwin) ;; Check if we are on macOS
    (let* ((phone-number
            (or (telephone-utils-get-number-at-point)
                (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                (read-string "Enter SMS recipient number: ")))
           (cleaned-number (telephone-utils-clean-phone-number phone-number))
           (formatted-number
            (cond
             ;; Local numbers (10 or fewer digits) - prepend default country code and '+'
             ((<= (length cleaned-number) 10)
              (concat "+" default-country-code cleaned-number))
             ;; Longer numbers (11+ digits) - ensure '+' prefix if default country code is used
             ((and (>= (length cleaned-number) 11)
                   (not (string-prefix-p "+" cleaned-number)))
              (concat "+" cleaned-number))
             ;; Otherwise, use the cleaned number as is
             (t
              cleaned-number)))
           (sms-message-text (read-string "Enter SMS message: ")))
      (when phone-number
        (message "Attempting to send SMS to: %s" formatted-number)
        (let ((apple-script
               (format
		"osascript -e 'tell application \"Messages\"' -e 'send \"%s\" to buddy \"%s\" of service \"SMS\"' -e 'end tell'"
		(shell-quote-argument sms-message-text)
		(shell-quote-argument formatted-number)))) ;; Quote for AppleScript and handle special chars
	  (ignore-errors ;; In case of errors in shell command execution
	    (message apple-script)
	    (shell-command apple-script)))
	(message "SMS sending command executed (check Messages app)."))))
  (unless (eq system-type 'darwin)
    (message "SMS sending is only supported on macOS using the Messages app.")))


(defun send-telegram-message ()
  "Open a Telegram chat with a phone number from point, region, or prompt.
  Prioritizes phone number at point, then region. If neither, prompts for input.
  For numbers with 10 digits or less, automatically prepends default country code.
  For numbers with 11+ digits, ensures they have a '+' prefix if default country code is used.
  Opens a Telegram chat window; message sending is manual in Telegram app/web."
  (interactive)
  (let* ((phone-number
	  (or (telephone-utils-get-number-at-point)
	      (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
	      (read-string "Enter Telegram recipient number: ")))
	 (cleaned-number (telephone-utils-clean-phone-number phone-number))
	 (formatted-number
	  (cond
	   ;; Local numbers (10 or fewer digits) - prepend default country code
	   ((<= (length cleaned-number) 10)
	    (concat "+" default-country-code cleaned-number)) ;; Use '+' for Telegram URL
	   ;; Longer numbers (11+ digits) - ensure '+' prefix if default country code is used
	   ((and (>= (length cleaned-number) 11)
		 (not (string-prefix-p "+" cleaned-number)))
	    (concat "+" cleaned-number))
	   ;; Otherwise, use the cleaned number as is
	   (t
	    cleaned-number)))
	 (telegram-url (concat "https://t.me/" formatted-number))) ;; Or "tg://resolve?domain=+" for app?

    (when phone-number
      (message "Opening Telegram chat with number: %s" formatted-number)
      (browse-url telegram-url))))


;;; Keybindings

(global-set-key (kbd "C-c C") 'call-telephone-number)
(global-set-key (kbd "C-c W") 'open-whatsapp-chat)
(global-set-key (kbd "C-c S") 'send-sms-message)
(global-set-key (kbd "C-c T") 'send-telegram-message)

(provide 'telephone-utils)

;;; telephone-utils.el ends here

