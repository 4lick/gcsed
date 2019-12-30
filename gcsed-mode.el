;;; gcsed-mode.el

;; This library implements the `gcsed-mode' minor mode, as well as
;; behavior for gcsed file and dired buffers.

;;; Code:

(require 'dash)
(require 'dired)

(require 'gcsed-util)
(require 'gcsed-io)

;; gcsed mode hooks
(defvar gcsed-mode-hook nil)
(defvar gcsed-mode-map (make-sparse-keymap))

(defun gcsed-is-active ()
  "Check whether gcsed is active."
  (and gcsed-mode (gcsed-string-starts-with default-directory gcsed-tmp-gcs-dir)))

(defun gcsed-after-save-hook ()
  "Push change to gcs after save."
  (when (gcsed-is-active)
    (gcsed-gcs-cp (buffer-file-name) (gcsed-buffer-gcs-path))))
(add-hook 'after-save-hook 'gcsed-after-save-hook)

(defun gcsed-before-revert-hook ()
  "Pull change before revert."
  (when (gcsed-is-active)
    (gcsed-gcs-cp (gcsed-buffer-gcs-path) (buffer-file-name))))
(add-hook 'before-revert-hook 'gcsed-before-revert-hook)

;;; dired functions

;; (defun gcsed-dired-do-gcs-delete (orig-dired-do-delete &rest args)
;; (defun gcsed-dired-do-gcs-flagged-delete (orig-dired-do-flagged-delete &rest args)

(defun gcsed-dired-do-gcs-refresh (orig-dired-do-refresh &rest args)
  "A wrapper around dired's dired-do-refresh function.
The original function and arguments are available as ORIG-DIRED-DO-REFRESH and ARGS."
  (when (and (gcsed-is-active) (gcsed-is-dired-active))
    (gcsed-refresh-tmp-dir))
  (apply orig-dired-do-refresh args))
(advice-add 'revert-buffer :around #'gcsed-dired-do-gcs-refresh)

;; (defun gcsed-dired-do-gcs-rename (orig-dired-do-rename &rest args)
;; (defun gcsed-dired-do-gcs-copy (orig-dired-do-copy &rest args)

(defun gcsed-dired-find-gcs-file (orig-dired-find-file &rest args)
  "A wrapper around dired's ‘dired-find-file’ function.
The original function and arguments are available as ORIG-DIRED-FIND-FILE and ARGS."
  (if (gcsed-is-active)
      (let* ((current-local-file (dired-get-filename)))
        (if (gcsed-is-directory current-local-file)
            (progn
              (gcsed-refresh-tmp-dir current-local-file)
              (apply orig-dired-find-file args))
          (progn
            (gcsed-gcs-cp (gcsed-local-path-to-gcs-path current-local-file) current-local-file)
            (apply orig-dired-find-file args))))
    (apply orig-dired-find-file args)))
(advice-add 'dired-find-file :around #'gcsed-dired-find-gcs-file)

;; (defun gcsed-dired-do-shell-command (orig-dired-do-shell-command &rest args)
;;   "A wrapper around dired's ‘dired-do-shell-command’ function.
;; The original function and arguments are available as ORIG-DIRED-DO-SHELL-COMMAND and ARGS."
;;   (if (gcsed-is-active)
;;       (let* ((current-local-files (if (dired-get-marked-files) (dired-get-marked-files)
;;                                     `(,(dired-get-filename))))
;;              (current-gcs-files (--map (gcsed-local-path-to-gcs-path it) current-local-files)))
;;         (if (seq-contains gcsed-streamable-commands (car (split-string (car args))))
;;             (if (-any? 'gcsed-is-directory current-local-files)
;;                 (message (format "%s: This command cannot be applied to an gcs directory" gcsed-app-name))
;;               (gcsed-gcs-cp-streams current-gcs-files (car args)))
;;           (message (format "%s: This command is not supported by %s" gcsed-app-name gcsed-app-name))))
;;     (apply orig-dired-do-shell-command args)))
;; (advice-add 'dired-do-shell-command :around #'gcsed-dired-do-shell-command)
;; 
;; (defun gcsed-dired-do-async-shell-command (orig-dired-do-async-shell-command &rest args)
;;   "A wrapper around dired's ‘dired-do-async-shell-command’ function.
;; The original function and arguments are available as ORIG-DIRED-DO-ASYNC-SHELL-COMMAND and ARGS."
;;   (if (gcsed-is-active)
;;       (let* ((current-local-files (if (dired-get-marked-files) (dired-get-marked-files)
;;                                     `(,(dired-get-filename))))
;;              (current-gcs-files (--map (gcsed-local-path-to-gcs-path it) current-local-files)))
;;         (if (seq-contains gcsed-streamable-commands (car (split-string (car args))))
;;             (if (-any? 'gcsed-is-directory current-local-files)
;;                 (message (format "%s: This command cannot be applied to an gcs directory" gcsed-app-name))
;;               (gcsed-gcs-cp-streams current-gcs-files (car args) t))
;;           (message (format "%s: This command is not supported by %s" gcsed-app-name gcsed-app-name))))
;;     (apply orig-dired-do-async-shell-command args)))
;; (advice-add 'dired-do-async-shell-command :around #'gcsed-dired-do-async-shell-command)

(define-minor-mode gcsed-mode
  "Minor mode for gcsed"
  :lighter " gcsed"
  :keymap gcsed-mode-map
  :global t)

(provide 'gcsed-mode)
