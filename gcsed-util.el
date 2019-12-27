;;; gcsed-util.el 

;; This library provides several functions commonly used throughout
;; gcsed

;;; Code:

(defconst gcsed-gcs-uri-scheme "gs://")

(defun gcsed-is-root-gcs-path (path)
  "Check if PATH is at the root of gcs."
  (equal path gcsed-gcs-uri-scheme))

(defun gcsed-string-starts-with (s prefix)
  "Return non-nil if string S begins with PREFIX."
      (cond ((>= (length s) (length prefix))
             (string-equal (substring s 0 (length prefix)) prefix))
            (t nil)))

(defun gcsed-string-ends-with (s suffix)
  "Return non-nil if string S ends with SUFFIX."
  (not (equal nil (string-match (format "%s\\'" suffix) s))))

(defun gcsed-shell-command-no-message (cmd &optional ret msg)
  "Run CMD, inhibiting messages from the Emacs builtin ‘shell-command’.
If 'RET' is not nil, results from CMD will be returned.
Default messages will be replaced with custom message 'MSG' if it is provided."
  (when msg (message msg))
  (let ((inhibit-message t))
    (if ret
        (shell-command-to-string cmd)
      (shell-command cmd))))

(defun gcsed-completing-read-backspace (cur-base)
  "If CUR-BASE is at the root, backspace acts normally.
Otherwise, backspace will go up one directory."
  (if (not (gcsed-is-root-gcs-path cur-base))
      (condition-case nil
          ;; use normal backspace behavior if no error was found
        (backward-delete-char 1)

        ;; if an error was found, we are at the beginning of the
        ;; line. Recurse to the parent directory of the current path.
        (error (throw 'backspace nil)))

    ;; if we're at the base, allow errors, don't recurse
    (backward-delete-char 1)))

;; finding files
(defun gcsed-completing-read (base msg)
  "Use ‘completing-read’ to find files in gcs starting at BASE.
MSG will be displayed to the user at prompt."
  (if (gcsed-string-starts-with base gcsed-gcs-uri-scheme)
      (let* ((choices (seq-remove (lambda (el) (not el)) (gcsed-gcs-ls base)))
             (choice (minibuffer-with-setup-hook
                         (lambda ()
                           (define-key (current-local-map) (kbd "<backspace>")
                             (lambda () (interactive) (gcsed-completing-read-backspace base))))
                       (catch 'backspace (completing-read (format "%s: %s" msg base) choices)))))

        ;; no choice means a backspace was entered, recurse upwards
        (if (not choice)
            (gcsed-completing-read (concat (mapconcat 'identity (butlast (butlast (split-string base "/"))) "/") "/")
                                     msg)
          (if (seq-contains choices choice)
              (if (and (not (string-match "/\\'" choice)))
                  (concat base choice)
                (gcsed-completing-read (concat base choice) msg))
            (concat base choice))))
    (let* ((choice (completing-read (format "%s: %s" msg base) `(,gcsed-gcs-uri-scheme "Elsewhere"))))
      (if (gcsed-is-root-gcs-path choice)
          (gcsed-completing-read choice msg)
        (read-file-name (format "%s: " msg) "")))))


(provide 'gcsed-util)

;;; gcsed-util.el ends here
