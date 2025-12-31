(defun collect-last-n-daily-entries (n)
  "Collect the previous N daily entries ending at POINT into a new buffer.
Assumes POINT marks the end of the daily logs (other info follows and must be excluded).
A daily entry starts with a heading line matching:
  * MM-DD-YYYY
and continues until the next such heading or the end boundary (POINT)."
  (interactive "nNumber of days: ")
  (let* ((date-heading-re "^\\* \\([0-1][0-9]-[0-3][0-9]-[0-9]\\{4\\}\\)")
         (dest (get-buffer-create "*weekly-report*"))
         ;; The end boundary is the current point, not necessarily (point-max).
         (end-boundary (point))
         (entries '()))
    (save-excursion
      ;; Start searching backward from the end boundary.
      (goto-char end-boundary)
      (let ((done nil))
        (dotimes (_ n)
          (unless done
            (if (re-search-backward date-heading-re nil t)
                (let ((start (point)))
                  ;; Determine the end of this entry by searching forward from start
                  ;; but never past end-boundary.
                  (save-excursion
                    (goto-char start)
                    ;; Move just past current heading line to avoid matching itself.
                    (forward-line 1)
                    (let (entry-end)
                      (if (re-search-forward date-heading-re end-boundary t)
                          (setq entry-end (match-beginning 0))
                        ;; No next heading before end-boundary; use end-boundary.
                        (setq entry-end end-boundary))
                      ;; Grab the entry text.
                      (push (buffer-substring-no-properties start entry-end) entries)))
                  ;; Move back one char to avoid rematching the same heading on next loop.
                  (when (> (point) (point-min))
                    (backward-char 1)))
              ;; No more headings found; stop early.
              (setq done t))))))
    ;; We collected from newest to oldest (due to backward search),
    ;; but we want newest-first in the destination buffer.
    (setq entries (nreverse entries))
    ;; Populate destination buffer.
    (with-current-buffer dest
      (read-only-mode -1)
      (erase-buffer)
      ;; Remove these instructional lines if you don't want them in the final output.
      (insert "Summarize the following daily work notes into a clean weekly report using plain 7-bit ASCII. Use these sections:\n")
      (insert "- KEY ACTIVITIES\n")
      (insert "- ISSUES & RESOLUTIONS\n")
      (insert "- MEETINGS\n")
      (insert "- NEXT STEPS\n")
      (insert "Do not include any special characters or UTF-8 symbols. Keep the formatting consistent with clear headers and dashed separators.\n\n")
      (insert "WEEKLY REPORT (last " (number-to-string (length entries)) " days)\n")
      (insert "----------------------------------------\n\n")
      (dolist (e entries)
        (insert e)
        ;; Ensure single blank line between entries.
        (unless (string-suffix-p "\n" e) (insert "\n"))
        (insert "\n"))
      (goto-char (point-min))
      (read-only-mode 1)
      ;; Copy the final buffer contents to the clipboard (via kill ring).
      (kill-new (buffer-string)))
    (message "Weekly report copied to clipboard.")
    (pop-to-buffer dest)))
