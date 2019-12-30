;;; gcsed.el --- Tramp-like access to GCS

;;; Code:

(require 'gcsed-mode)

;; define entry points to gcsed - gcsed-find-file

(defun gcsed-find-file ()
  "Open gcsed buffer at input-file.
Will be a refreshed dired buffer if it is a directory."
  (interactive)
  (if gcsed-mode
      (let* ((current-gcs-base-path (if (gcsed-is-active)
                                       (gcsed-local-path-to-gcs-path default-directory)
                                     "gs://"))
             (current-gcs-file-path (gcsed-completing-read current-gcs-base-path
                                                            "Find GCS file"))
             (current-local-file-path (gcsed-gcs-path-to-local-path current-gcs-file-path)))
        (if (gcsed-is-directory current-gcs-file-path)
            (progn
              (gcsed-refresh-tmp-dir current-local-file-path)
              (dired current-local-file-path))
          (progn
            (gcsed-gcs-cp (gcsed-local-path-to-gcs-path current-local-file-path)
                           current-local-file-path)
            (find-file current-local-file-path))))
    (when (y-or-n-p "gcsed mode is disabled, do you want to enable gcsed? ")
      (gcsed-mode)
      (gcsed-find-file))))
