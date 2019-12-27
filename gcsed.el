;;; gcsed.el --- Tramp-like access to GCS

;;; Code:

(require 'gcsed-io)

;; define entry points to gcsed - gcsed-find-file

(defun gcsed-find-file (path)
  "Open gcsed buffer at input-file."
  (interactive)
  (gcsed-gcs-ls path))
