;;; gcsed-io.el --- I/O operations both locally and on GCS

;; This library provides functions for interacting directly with files on
;; gcs, as well as some functions for interactinve with local files.

;;; Code:

(require 'dash)

(require 'gcsed-util)

(defconst gcsed-app-name "gcsed")
(defconst gcsed-tmp-gcs-dir (concat "/tmp/" gcsed-app-name))

;; gcs functions

(defun gcsed-get-transfer-message (src dest async)
  "Get message to display when transferring data from SRC to DEST.  If specified, this will be an ASYNC operation."
  (format "%s: Transferring data %s gcs%s..." gcsed-app-name
          (if (gcsed-is-gcs-path src) (if (gcsed-is-gcs-path dest)
                                           "within" "from") "to")
          (if async " in the background" "")))

(defun gcsed-gcs-ls (path)
  "List an gcs PATH."
  (cl-flet ((parse-gcs-ls-raw-output (raw-line) (car (-take-last 1 (split-string it)))))
    (--remove (or (string= "" it) (not it))
              (--map (if (gcsed-is-root-gcs-path path)
                         (concat (parse-gcs-ls-raw-output it) "/")
                       (parse-gcs-ls-raw-output it))
                     (split-string (gcsed-shell-command-no-message
                                    (format "gsutil ls %s" path) t
                                    (format "%s: Listing files on gcs..." gcsed-app-name)) "\n")))))

(defun gcsed-gcs-cp (src dest &optional async)
  "Copy gcs SRC file to DEST.  If specified, this will be a ASYNC operation."
  (let ((command (format "gsutil cp %s %s" src dest))
        (msg (gcsed-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "gcsed-cp" "*gcsed*" (split-string command))
          (message msg))
      (gcsed-shell-command-no-message command :msg msg))))

(defun gcsed-gcs-cp-streams (srcs command &optional async)
  "Stream gcs SRCS into COMMAND.  If specified, this will be an ASYNC operation."
  (let ((command (format "(%s) | %s" (mapconcat 'identity (--map (format "gsutil cp %s -;" it)
                                                                 srcs) " ") command)))
    (if async
        (async-shell-command command)
      (shell-command command))))

(defun gcsed-gcs-mv (src dest &optional async)
  "Move gcs SRC file to DEST.  If specified, this will be a ASYNC operation."
  (let ((command (format "gsutil mv %s %s" src dest))
        (msg (gcsed-get-transfer-message src dest async)))
    (if async
        (progn
          (apply 'start-process "gcsed-mv" "*gcsed*" (split-string command))
          (message msg))
      (gcsed-shell-command-no-message command :msg msg))))

(defun gcsed-gcs-rm (path &optional async)
  "Remove file or directory PATH from gcs. If specified, this will be a ASYNC operation."
  (let ((msg (format "%s: Removing data from gcs%s..." gcsed-app-name
                     (if async " in the background" "")))
        (command (format "gsutil rm %s %s" path)))
    (if async
        (progn
          (apply 'start-process "gcsed-rm" "*gcsed*" (split-string command))
          (message msg))
      (gcsed-shell-command-no-message command :msg msg))))

;; local filesystem functions

(defun gcsed-mkdirs (paths)
  "Create directories at the given PATHS."
  (dolist (subgroup (-partition-all 100 paths))
    (gcsed-shell-command-no-message (format "mkdir -p %s" (mapconcat 'identity subgroup " "))
                                      :msg "gcsed: Creating local directories...")))

(defun gcsed-create-empty-file (filename)
  "Create FILENAME if it doesn't exist."
  (gcsed-shell-command-no-message (format "mkdir -p %s && touch %s" (gcsed-parent-directory filename)
                                            filename)
                                    :msg "gcsed: Creating dummy file..."))

(defun gcsed-create-empty-files (filenames)
  "Create all files in FILENAMES if they don't exist."
  (dolist (subgroup (-partition-all 100 filenames))
    (gcsed-mkdirs (--map (gcsed-parent-directory it) subgroup))
    (gcsed-shell-command-no-message (format "touch %s"
                                              (mapconcat 'identity subgroup " "))
                                      :msg "gcsed: Creating dummy files...")))

(defun gcsed-rm (file-or-directory)
  "Recursively remove FILE-OR-DIRECTORY.
This will only run if FILE-OR-DIRECTORY is in the gcsed-tmp-gcs-dir."
  (when (gcsed-string-starts-with file-or-directory gcsed-tmp-gcs-dir)
    (shell-command (format "rm -rf %s" file-or-directory))))

;; validation

(defun gcsed-is-gcs-path (path)
  "Confirm that this PATH is a valid gcs path."
  (gcsed-string-starts-with path "gs"))

(defun gcsed-is-directory (path)
  "Confirm that this PATH is a directory."
  (if (gcsed-is-gcs-path path)
      (gcsed-string-ends-with path "/")
    (equal 0 (gcsed-shell-command-no-message (format "test -d %s" path)))))

;; gcs to local translation, path functions

(defun gcsed-local-path-to-gcs-path (path)
  "Convert local PATH to an gcs path."
  (let ((gcs-path (replace-regexp-in-string gcsed-tmp-gcs-dir "gs:/" path)))
    (when (gcsed-is-gcs-path gcs-path)
      (if (and (gcsed-is-directory path) (not (gcsed-string-ends-with gcs-path "/")))
          (concat gcs-path "/")
        gcs-path))))

(defun gcsed-gcs-path-to-local-path (path)
  "Convert gcs PATH to a local path."
  (when (gcsed-is-gcs-path path)
    (let ((local-path (replace-regexp-in-string "gs:/" gcsed-tmp-gcs-dir path)))
      local-path)))

(defun gcsed-buffer-gcs-path ()
  "Get associated gcs path of current buffer."
  (gcsed-local-path-to-gcs-path (buffer-file-name)))

(defun gcsed-parent-directory (path)
  "Get parent directory path of PATH."
  (if (gcsed-string-ends-with path "/")
      (concat (mapconcat 'identity (-drop-last 2 (split-string path "/")) "/") "/")
    (concat (mapconcat 'identity (-drop-last 1 (split-string path "/")) "/") "/")))

;; Refresh

(defun gcsed-refresh-tmp-dir (&optional input-dir)
  "Refresh all active gcsed buffers, including INPUT-DIR if provided."
  (let* ((all-active-files-dirs (--map (with-current-buffer it (if (gcsed-is-dired-active)
                                                                   default-directory
                                                                 buffer-file-name))
                                       (buffer-list)))
         (all-gcsed-files-dirs (--separate (gcsed-is-directory
                                              (gcsed-local-path-to-gcs-path it))
                                             (--filter (gcsed-string-starts-with
                                                        it gcsed-tmp-gcs-dir)
                                                       (add-to-list 'all-active-files-dirs
                                                                    input-dir))))
         (active-directory (condition-case nil (if (gcsed-is-dired-active) default-directory
                                                          (gcsed-parent-directory buffer-file-name))
                                      (error nil))))
    (when active-directory (make-directory active-directory t))
    ;;(gcsed-rm gcsed-tmp-gcs-dir)

    (dolist (current-directory (car all-gcsed-files-dirs))
      (make-directory current-directory t)
      (when (or (equal current-directory active-directory) (equal current-directory input-dir))
          (let* ((gcs-directory (gcsed-local-path-to-gcs-path current-directory))
                 (file-list (-filter (lambda (f) f) (gcsed-gcs-ls gcs-directory)))
                 (full-gcs-paths (-map (lambda (file) (concat gcs-directory file))
                                      file-list))
                 (organized-file-list (--separate (gcsed-is-directory it) full-gcs-paths))
                 (gcs-dirs (-map (lambda (f) (gcsed-gcs-path-to-local-path f))
                                (car organized-file-list)))
                 (gcs-files (-map (lambda (f) (gcsed-gcs-path-to-local-path f))
                                 (car (-take-last 1 organized-file-list)))))

            ;; reset local tmp directory and delete the current directory
            ;; rebuild from files gcs
            (when gcs-dirs (gcsed-mkdirs gcs-dirs))
            (when gcs-files (gcsed-create-empty-files gcs-files)))))

    (dolist (current-file (car (-take-last 1 all-gcsed-files-dirs)))
      (let ((gcs-file (gcsed-local-path-to-gcs-path current-file)))
        (gcsed-gcs-cp gcs-file current-file)))))

(provide 'gcsed-io)

;;; gcsed-io.el ends here
