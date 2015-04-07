;;; gofmt-replace --- Replacing gofmt from go-mode.el
;;; Commentary:
;;; Instead of writing into a temporary file and using diff we directly apply gofmt
;;; Code:
(defun revert-buffer-no-confirm ()
      "Revert buffer without confirmation."
      (interactive)
      (revert-buffer t t))

(defun gofmt ()
  "Format the current buffer according to the gofmt tool."
  (interactive)
  (call-process gofmt-command nil t nil "-w" buffer-file-name)
  (message "Applied gofmt")
  (revert-buffer-no-confirm)
)

(provide 'gofmt-replace)
;;; gofmt-replace.el ends here
