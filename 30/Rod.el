;; Define a function to force Unix line ends for python.
(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))
(define-key global-map (kbd "<f6>") 'dos2unix)

;; define a function to force line ends to dos.
(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))
(define-key global-map (kbd "<f7>") 'unix2dos)

(defun insert-morning-items ()
  "Insert the standard morning items into my did file."
  (interactive)
  (insert "Ran the morning accounts process.\n\nPulled and consolidated the MDF Temp report. Sent the report to Ashley and Chris W."))
(global-set-key (kbd "<f6>") 'insert-morning-items)

(defun insert-accounts-process ()
  "Insert the standard accounts process item"
  (interactive)
  (insert "Ran the afternoon acounts process.\n\n"))
(global-set-key (kbd "<f7>") 'insert-accounts-process)


;; Setup a key to turn on line numbering
(define-key global-map (kbd "<f8>") 'linum-mode)

;; Keep a list of recently visited files and put them in a menu across sessions.
(require 'recentf)
(recentf-mode 1)

;; Rod Customizations
(setq display-time-day-and-date t)  ;; Display time and date on status line
(display-time)

; display column numnber on status line
(setq column-number-mode t)

; set default frame size
(when window-system  
  (set-frame-size (selected-frame) 110 34)   ; set the size if the frame
  (set-frame-position (selected-frame) 1 1)) ; put the frame at top left of screen pixtal 1 1

(defun rodfixframe()
  (interactive)
  (set-frame-size (selected-frame) 110 34) ; rows and columns w h
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq calendar-latitude 39.203714)
(setq calendar-longitude -76.860276)
(setq calendar-location-name "Columbia, MD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rod's Custom Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup a special function to open my rod.rod.org file for logging.
(defun rodstart()
  "This function will open roddid and setup for an entry for today."
  (interactive)
  ;;;; (find-file "$USERPROFILE/Documents/Rod.did.org")
  (find-file "/mnt/c/Users/RCLAYTON/Documents/Org/Rod.did.org")
  (goto-char (point-min)) ; In case we run it while the file is open.
  (if (search-forward (concat "* " (format-time-string "%m-%d-%Y")) nil t)
    (progn
      (org-beginning-of-line)
      (org-show-entry)
      (search-forward-regexp "^\*")
      (search-forward-regexp "^\*")
      (forward-line -1) ; go up ine line.
      (end-of-line))
    (progn
      (end-of-buffer)
      (search-backward-regexp "^\* [[:digit:]]+-[[:digit:]]+-[[:digit:]]+")
      (org-cycle)
      (end-of-line)
      (search-forward-regexp "^\*")
      (goto-char (- (point) 2))
      (org-insert-heading)
      (insert (format-time-string "%m-%d-%Y") " "))))

; Open rod.org to the current date ready for new entries
(define-key global-map (kbd "<f5>") 'rodstart)

; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

; define key F12 to start calc
(define-key global-map (kbd "<f12>") 'calc)

; Used to convert Teams Texts from Susan for DHCP reservations to tab delimited and correct MAC format.
(defun convert-to-tabbed-mac-dash-lower (start end)
  "Convert selected text to tab-separated format, replace colons in MAC address with dashes, and convert MAC to lowercase."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (parts (split-string text))
         (mac (downcase (replace-regexp-in-string ":" "-" (nth 2 parts))))
         (new-text (mapconcat 'identity (list (nth 1 parts) (nth 0 parts) mac) "\t")))
    (delete-region start end)
    (insert new-text)))

; get rod did entries from the last n days for weekly accomplishments report
(load-file "/home/rod/.emacs.d/collect-last-n-daily-entries.el")

