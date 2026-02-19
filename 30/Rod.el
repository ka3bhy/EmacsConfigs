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
(defun rodstart ()
  "Open Rod.did.org and append to today's entry.

If a top-level heading starting with today's date exists (e.g. \"* 02-18-2026\"
or \"* 02-18-2026 Out Sick\"), reveal it and append at the end of its entry
(right before the next top-level heading).

If it does not exist, insert a new \"* MM-DD-YYYY\" heading immediately ABOVE
the star separator line (\"* ******...\"), which marks the start of the footer/TODO area."
  (interactive)
  (let* ((file "/mnt/c/Users/RCLAYTON/Documents/Org/Rod.did.org")
         (today (format-time-string "%m-%d-%Y"))
         ;; Matches headings like:
         ;;   * 02-17-2026
         ;;   * 02-18-2026 Out Sick
         (today-re (concat "^\\* " (regexp-quote today) "\\b"))
         ;; Your footer boundary line:
         ;;   * *******************************************************************
         (starline-re "^\\* \\*\\{10,\\}.*$"))
    (find-file file)
    (goto-char (point-min))

    (if (re-search-forward today-re nil t)
        ;; ---------------- Found today's heading: append to end of entry ----------------
        (progn
          (org-back-to-heading t)
          (org-show-entry)
          (org-show-subtree)

          ;; Move to the end of today's entry (before next top-level heading).
          (forward-line 1)
          (if (re-search-forward "^\\* " nil t)
              (progn
                (beginning-of-line)
                ;; Remove trailing blank lines so we append neatly.
                (skip-chars-backward "\n")
                (end-of-line)
                (insert "\n"))
            ;; No next heading: append at end of buffer.
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))))

      ;; ---------------- Not found: insert new heading ABOVE star line ----------------
      (progn
        (goto-char (point-min))
        (if (re-search-forward starline-re nil t)
            (beginning-of-line)
          ;; If star line not found for some reason, fall back to end-of-file.
          (goto-char (point-max))
          (unless (bolp) (insert "\n")))

        ;; Keep formatting tidy: ensure a blank line before the new heading.
        (unless (or (bobp) (looking-back "\n\n" nil))
          (insert "\n"))

        ;; Insert new date heading and place point on the blank body line ready to type.
        (insert "* " today "  \n")
        ;; put the back the cursor up to the end of the previous line with the date.
        (backward-char)))))

; Open rod.org to the current date ready for new entries
(define-key global-map (kbd "<f5>") 'rodstart)

; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

; define key F12 to start calc
(define-key global-map (kbd "<f12>") 'calc)

; Used to convert Teams Texts from Susan for DHCP reservations (HSM) to tab delimited and correct MAC format.
(defun convert-to-tabbed-mac-dash-lower (start end)
  "Convert selected text to tab-separated format, replace colons in MAC address with dashes, and convert MAC to lowercase."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (parts (split-string text))
         (mac (downcase (replace-regexp-in-string ":" "-" (nth 2 parts))))
         (new-text (mapconcat 'identity (list (nth 1 parts) (nth 0 parts) mac) "\t")))
    (delete-region start end)
    (insert new-text)))

(defun rodney/diameter-from-gauge (gauge)
  "Compute Diameter (inches) ≈ 1.6699 × (1 / ∛Gauge).

Interactively prompts for GAUGE (a positive number) and displays
the resulting diameter in the echo area."
  (interactive "nEnter Gauge (positive number): ")
  (unless (and (numberp gauge) (> gauge 0))
    (user-error "Gauge must be a positive number"))
  (let* ((diameter (/ 1.6699 (expt gauge (/ 1.0 3.0)))))
    (message "Diameter (inches) ≈ %.6f  (Gauge=%g)" diameter gauge)
    diameter))

; get rod did entries from the last n days for weekly accomplishments report
(load-file "/home/rod/.emacs.d/collect-last-n-daily-entries.el")

; Setup abbrev mode for org mode
(load-file "/home/rod/.emacs.d/abbrev.el")
