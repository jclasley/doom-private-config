(defun yank-file-name ()
  "If the buffer is visiting a file, yank the FILENAME and POSITION"
  (interactive)
  (if-let ((f (buffer-file-name))
           (p (line-number-at-pos)))
      (progn
        (kill-new (format "%s:%s" f p))
        (message "Copied"))
    (message "Not a valid buffer")))

(defun foo (&rest args)
  (plist-get args :bar))

(defun org-roam-daily--daphne ()
  (interactive)
  (org-roam-dailies-goto-today))
