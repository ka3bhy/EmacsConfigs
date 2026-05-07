
;; Setup abbrav mode for org mode with abbrivations.
(add-hook 'org-mode-hook (lambda () (abbrev-mode t)))

; Rod's standard org mode abbreviations
(define-abbrev org-mode-abbrev-table "macct" "Ran the morning accounts process.")
(define-abbrev org-mode-abbrev-table "mdfrpt" "Pulled and consolidated the MDF Temp report. Sent the report to Ashley and Chris W.")
(define-abbrev org-mode-abbrev-table "aacct" "Ran the afternoon accounts process.")
(define-abbrev org-mode-abbrev-table "nophones" "There were no phones on the wrong VLAN.")
(define-abbrev org-mode-abbrev-table "cwh" "Chris White")
(define-abbrev org-mode-abbrev-table "crom" "Chris Rommel")

;; experimental things
                                        ;
; define a function to display the current date
(defun my-current-date ()
  (insert (format-time-string "* %m-%d-%Y") " "))

; call the function above with abbriv mode
(define-abbrev org-mode-abbrev-table "mydate" "" 'my-current-date)
