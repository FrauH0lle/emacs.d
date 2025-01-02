;; lisp/autoload/tools-backups.el -*- lexical-binding: t; -*-

;; This file makes Emacs' backup system a bit more useful. It implements a new
;; major mode `zenit-file-backups-mode' and the minor mode
;; `+backups/list-backups' which lists all available local backups for an open
;; file. These backups can be viewed, diff'ed and if desired, used to revert the
;; file.
;;
;; This implementation is based on
;; https://github.com/chadbraunduin/backups-mode, but slimmed down and adjusted
;; to my liking.

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-remove-duplicates "cl-seq" (cl-seq &rest cl-keys))

;; `evil-states'
(declare-function evil-emacs-state "ext:evil-states" t t)

;; `subr-x'
(declare-function hash-table-keys "subr-x" (hash-table))
(autoload #'hash-table-keys "subr-x")


;;
;;; Variables

(defvar +backups-files-function #'+backups--backup-files
  "Function used to return the backup files.

This variable should be set to a function that takes a file name
as an argument and returns a list of backup files for that file.
The default value is `+backups--backup-files'.")

(defvar +backups--saved-wconf nil
  "Store the window configuration of the before activating
`zenit-file-backups-mode' or `zenit-view-backup-file-mode'.")

;;
;;; File versions

(defun +backups--full-version-number (file-name start &optional number-str)
  "Extract the version number from the FILE-NAME.

The version number is constructed by concatenating all
consecutive numeric characters found in the FILE-NAME after
START.

NUMBER-STR is an optional argument that represents an accumulated
version number string. It is used internally by the function to
build the final version number.

The function returns the extracted version number as an integer.
If any non-numeric characters are encountered in the FILE-NAME,
the function returns 0."
  (let* ((number-str (or number-str ""))
         (number (string-to-number number-str)))
    (if (< start (length file-name))
        (let ((current-char (substring file-name (1+ start) (+ 2 start))))
          (if (and (stringp current-char)
                   (string-match-p "[0-9]+" current-char))
              (+backups--full-version-number file-name (1+ start) (concat number-str current-char))
            number))
      number)))

(defun +backups--make-version-number (file-name)
  "Extract the version number from the FILE-NAME.
The version number is assumed to be the sequence of numeric
characters found at the end of the FILE-NAME and enclosed by
tilde characters (e.g. \"~123~\").

The function returns the extracted version number as an integer,
or nil if no version number is found in the FILE-NAME."
  (let ((version-start (string-match "~[0-9]+~$" file-name)))
    (when version-start
      (+backups--full-version-number file-name version-start))))


(defun +backups--file-sort-p (file-name1 file-name2)
  "Compare the version numbers of two backup files.
The version number of a file is assumed to be the sequence of
numeric characters found at the end of the file name and enclosed
by tilde characters (e.g. \"~123~\").

The function returns t if the version number of FILE-NAME1 is
greater than that of FILE-NAME2, and nil otherwise."
  (let ((version1 (+backups--make-version-number file-name1))
        (version2 (+backups--make-version-number file-name2)))
    (when (and version1 version2)
      (> version1 version2))))

(defun +backups--get-last-modified (file)
  "Return the last modified date of FILE in the format \"%Y-%m-%d
%T\". The function uses `file-attributes' to retrieve the file's
information, and `format-time-string' to format the last modified
date."
  (and (file-exists-p file)
       (format-time-string "%Y-%m-%d %T"
                           (nth 5 (file-attributes file)))))

(defun +backups--make-file (file-name)
  "Return a list with the version number, last modified date, and
filename for FILE-NAME. The version number is obtained from
`+backups--make-version-number', and the last modified date is
obtained from `+backups--get-last-modified'.

The returned list has the following format: (VERSION-NUMBER
LAST-MODIFIED-DATE FILENAME)"
  (list (+backups--make-version-number file-name)
        (+backups--get-last-modified file-name)
        file-name))

(defun +backups--backup-files (original-file)
  "Return a list of backup files for the `original-file`.

The function uses `make-backup-file-name' to get the name of the
backup file for `original-file`, and `file-name-all-completions'
to get a list of all backup files in the same directory as the
backup file. The returned list consists of the full path of each
backup file."
  (let* ((backup-file (file-name-sans-versions
                       (make-backup-file-name (expand-file-name original-file))))
         (backup-directory (file-name-directory backup-file)))
    (mapcar (lambda (f) (concat backup-directory f))
            (file-name-all-completions
             (file-name-nondirectory backup-file)
             backup-directory))))

(defun +backups--get-sorted-backups (original-file &optional backups-find-files-fn)
  "Return a list of backup files of ORIGINAL-FILE, sorted in
 descending order of their version numbers.

The function uses the function specified in the optional argument
BACKUPS-FIND-FILES-FN to find the backup files. If this argument
is not provided, the value of `+backups-files-function' is used.
The sorted list of files is created by mapping the function
`+backups--make-file' over the list of backup files, which also
includes the original file."
  (cl-remove-duplicates
   (mapcar '+backups--make-file
           (cons original-file
                 (sort
                  (funcall (or backups-find-files-fn +backups-files-function) original-file)
                  #'+backups--file-sort-p)))
   :test #'equal))


;;
;;; List backups

(defvar-local +backups--file-info-alist nil
  "An alist that stores information about the opened file and its
available backups.")

(defvar-local +backups--first-diff-index nil
  "Stores the index of the first difference in the sorted backups.")

(defvar-local +backups--assoc-files-alist nil
  "An alist of filenames of associated backup files.")

(defun +backups--get-backups (data)
  "Return the list of backups stored in the data alist.

The data alist is expected to have a key `:backups' whose value
represents the list of backups.

This function returns the value of the `:backups' key from the
data alist, or nil if the key is not present."
  (cdr (assq :backups data)))

(defun +backups--get-version (file)
  "Return the version number of the backup FILE.
FILE is a list of the form (VERSION DATE FILENAME)"
  (nth 0 file))

(defun +backups--get-last-modified-date (file)
  "Return the last modification date of the backup FILE.
FILE is a list of the form (VERSION DATE FILENAME)"
  (nth 1 file))

(cl-defun +backups--list-backups-from-file (original-file &key data)
  "Create a buffer displaying information about the backups of a
file. ORIGINAL-FILE is the name of the original file.

The function populates the buffer with information about the
available backups of the file, including the version and
last-modified date of each backup.

The information about the original file and its backups is stored
in the buffer-local variables `+backups--file-info-alist',
`+backups--first-diff-index', and `+backups--assoc-files-alist'.

KEY is an optional alist that contains information about the
original file and its backups. If it is not provided, the
function creates a new alist with the required information.

The created buffer is displayed in the `zenit-file-backups-mode'
and is read-only."
  (setq +backups--saved-wconf (current-window-configuration))
  (let ((buf (format "*Backups: %s*" (buffer-name (get-file-buffer original-file)))))
    (with-current-buffer (get-buffer-create buf)
      (pop-to-buffer (current-buffer))
      (erase-buffer)
      (zenit-file-backups-mode)
      (when (featurep 'evil)
        (evil-emacs-state +1))
      (read-only-mode -1)

      (unless (assq :original-file data)
        (push `(:original-file . ,original-file) data))
      (push `(:backups . ,(+backups--get-sorted-backups original-file +backups-files-function)) data)
      (push `(:backups-buffer . ,(current-buffer)) data)

      (setq +backups--file-info-alist data)
      (setq +backups--first-diff-index nil)

      (setq +backups--assoc-files-alist
            (mapcar (lambda (element) (nth 2 element)) (cdr (assq :backups data))))

      (insert (format "%s\n" original-file))
      (insert (mapconcat
               (lambda (file)
                 (let* ((version (+backups--get-version file))
                        (version (if version (number-to-string version) "current"))
                        (last-modified-date (or (+backups--get-last-modified-date file) "unknown\t")))
                   (format "  %-6s\t%s\n"
                           (propertize version 'face 'font-lock-keyword-face)
                           last-modified-date)))
               (+backups--get-backups data)
               ""))

      (goto-char 1)
      (forward-line)
      (set-buffer-modified-p nil)
      (read-only-mode 1))))



;;
;;; Cleanup

(defun +backups--cleanup-and-close ()
  "Cleanup open buffers and close the backup session.

This function closes the buffer displaying information about the
backups of a file and kills any buffers associated with the
backups. It then restores the previous window configuration."
  (mapc (lambda (buffer)
          (when (buffer-live-p (get-file-buffer buffer))
            (kill-buffer (get-file-buffer buffer))))
        (cdr +backups--assoc-files-alist))
  (kill-buffer-and-window)
  (set-window-configuration +backups--saved-wconf))


;;
;;; Modes

;; Major mode
(defvar zenit-file-backups-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'+backups/diff)
    (define-key map (kbd "RET") #'+backups/view)
    (define-key map (kbd "R") #'+backups/revert)
    (define-key map (kbd "q") (lambda (&rest _)
                                (interactive)
                                (+backups--cleanup-and-close)))
    map))

(define-derived-mode zenit-file-backups-mode special-mode
  "Backups"
  "Major mode for viewing, inspecting and reverting of a buffer's
 backup files.

The mode provides functionality for viewing information about the
backups of a file, including the version and last-modified date
of each backup."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local header-line-format (concat "<return> view backup, <d> + <d> diff, <R> revert, <q> quit"))
  (setq +backups--file-info-alist nil)
  (setq +backups--first-diff-index nil)
  (setq +backups--assoc-files-alist nil))

;; Minor mode
(defvar +backups--minor-saved-wconf nil
  "Store the window configuration of the before activating
`zenit-view-backup-file-mode'.")

(defun +backups/close-view-buffer ()
  "Close the buffer viewing the backup file.

This function kills the buffer viewing the backup file and
restores the previous window configuration."
  (interactive)
  (kill-buffer-and-window)
  (set-window-configuration +backups--minor-saved-wconf))

(define-minor-mode zenit-view-backup-file-mode
  "Minor mode for viewing a single backup file."
  :init-value nil
  :lighter " Backup-file"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'+backups/close-view-buffer)
            map)
  (setq-local header-line-format "Press <q> to exit buffer.")
  (when (featurep 'evil)
    (evil-emacs-state)))


;;
;;; Commands

(defun +backups--get-file-name (file)
  "Return the name of the backup file specified by FILE.
FILE is a list of the form (VERSION DATE FILENAME)."
  (nth 2 file))

(defun +backups--get-file-name-from-index (index)
  "Return the file name of the backup file at INDEX.

This function retrieves the backup file information from
`+backups--file-info-alist' and uses the
`+backups--get-file-name'function to extract the file name from
the information."
  (+backups--get-file-name (nth index (+backups--get-backups +backups--file-info-alist))))

(defun +backups--get-index-number (line-number)
  "Return the index number of the backup file in the backups list.

LINE-NUMBER is the line number in the buffer where the
information about the backup file is displayed. The first line in
the buffer displays the name of the original file, and the
subsequent lines display information about the backups. The index
number is calculated as the difference between the LINE-NUMBER
and 2, since the first line in the buffer is not counted in the
index."
  (- line-number 2))

(defun +backups--get-line-number (index)
  "Return the line number for the backup at INDEX.

This function takes an INDEX (a zero-based index into the list of
backups for a file), and returns the corresponding line number in
the buffer displaying the backups. The first line of the buffer
corresponds to line number 1."
  (+ index 2))

(defun +backups--get-original-file (data)
  "Return the original file from the backups information in DATA.

DATA is an alist that contains information about the original
file and its backups.

The function returns the value associated with the key
`:original-file' in the alist."
  (cdr (assq :original-file data)))

(defun +backups/diff ()
  "Diff two versions of the file."
  (interactive)
  ;; Remove write protection
  (read-only-mode -1)
  (let* ((line-number (line-number-at-pos))
         (index (+backups--get-index-number line-number))
         (orig-buffer-name (buffer-name (get-file-buffer (+backups--get-original-file +backups--file-info-alist)))))
    (if (and (>= index 0) (< index (length (+backups--get-backups +backups--file-info-alist))))
        (progn
          (cond ((eq +backups--first-diff-index index)
                 (beginning-of-line)
                 (delete-char 1)
                 (insert " ")
                 (setq +backups--first-diff-index nil)
                 (beginning-of-line))
                (+backups--first-diff-index
                 (forward-line (- (+backups--get-line-number +backups--first-diff-index) line-number))
                 (delete-char 1)
                 (insert " ")
                 (forward-line (- line-number (+backups--get-line-number +backups--first-diff-index)))
                 (progn
                   (when (and
                          (zerop +backups--first-diff-index)
                          (get-buffer orig-buffer-name)
                          (buffer-modified-p (get-buffer orig-buffer-name)))
                     (let ((backups-mode-buffer-name (buffer-name)))
                       (switch-to-buffer orig-buffer-name)
                       (save-buffer)
                       (switch-to-buffer backups-mode-buffer-name)))
                   (let ((first-file-name (+backups--get-file-name-from-index +backups--first-diff-index))
                         (second-file-name (+backups--get-file-name-from-index index)))
                     (setq +backups--first-diff-index nil)
                     (set-buffer-modified-p nil)
                     (ediff first-file-name second-file-name))))
                (t
                 (setq +backups--first-diff-index index)
                 (beginning-of-line)
                 (insert "d")
                 (delete-char 1)
                 (forward-line)))
          (set-buffer-modified-p nil))
      (error "No file selected.")))
  ;; Enable write protection
  (read-only-mode 1))

(defun +backups--revert-backup-from-file (orig-file-name backup-file-name)
  "Revert a backup file to the original file.

This function is used to revert a backup file to the original
file. It asks the user for confirmation before making any
permanent changes. If the user confirms, it creates a temporary
copy of the backup file, saves the current buffer associated with
the original file, and then copies the temporary copy over the
original file, effectively reverting the file to the state stored
in the backup file. Finally, it deletes the temporary copy.

ORIG-FILE-NAME is the name of the original file.

BACKUP-FILE-NAME is the name of the backup file to be reverted."
  (let* ((temp-backup-file-name (concat backup-file-name "#temp#"))
         (orig-buffer-name (buffer-name (get-file-buffer orig-file-name))))
    ;; Ask before reverting
    (when (y-or-n-p "Do you really want to revert this file permanently?")
      ;; Create temporary copy
      (copy-file backup-file-name temp-backup-file-name t)
      ;; Save current file
      (when orig-buffer-name
        (with-current-buffer orig-buffer-name
          (save-buffer)))
      ;; Copy temporary copy over original file
      (copy-file temp-backup-file-name orig-file-name t)
      ;; Remove temporary copy
      (delete-file temp-backup-file-name))))

(defun +backups/revert ()
  "Revert current file to chosen backup."
  (interactive)
  (let ((index (+backups--get-index-number (line-number-at-pos))))
    (cond ((zerop index)
           (error "Cannot revert current file."))
          ((and (> index 0) (< index (length (+backups--get-backups +backups--file-info-alist))))
           (+backups--revert-backup-from-file (+backups--get-original-file +backups--file-info-alist)
                                              (+backups--get-file-name-from-index index)))
          (t (error "No file selected.")))))

(defun +backups--guess-mode (filename)
  "Returns major-mode for FILENAME based on `auto-mode-alist' or
nil. Adapted from `set-auto-mode--apply-alist'"
  (if (file-name-case-insensitive-p filename)
      ;; Filesystem is case-insensitive.
      (let ((case-fold-search t))
        (assoc-default filename auto-mode-alist 'string-match))
    ;; Filesystem is case-sensitive.
    (or
     ;; First match case-sensitively.
     (let ((case-fold-search nil))
       (assoc-default filename auto-mode-alist 'string-match))
     ;; Fallback to case-insensitive match.
     (and auto-mode-case-fold
          (let ((case-fold-search t))
            (assoc-default filename auto-mode-alist 'string-match))))))

(defun +backups/view ()
  "View a single backup file."
  (interactive)
  (setq +backups--minor-saved-wconf (current-window-configuration))
  (let ((index (+backups--get-index-number (line-number-at-pos))))
    (cond ((zerop index)
           (error "Cannot view current file."))
          ((and (> index 0) (< index (length (+backups--get-backups +backups--file-info-alist))))
           (let ((file-name (+backups--get-file-name-from-index index))
                 (mode (or (+backups--guess-mode (+backups--get-original-file +backups--file-info-alist))
                           'fundamental-mode)))
             (find-file-read-only-other-window file-name)
             (funcall mode)
             (zenit-view-backup-file-mode +1)))
          (t (error "No file selected.")))))

;;;###autoload
(defun +backups/list-backups ()
  "Lists all saved backups for the current file."
  (interactive)
  (let ((original-file (buffer-file-name)))
    (if original-file
        (+backups--list-backups-from-file original-file)
      (error "No backups for a non-file visiting buffer."))))

(defun +backups--collect-orphans ()
  "Return list of orphans.
The list has the form (ORGIN-FNAME . HASHED-FNAME)."
  (let* ((cache-fname (file-name-concat zenit-cache-dir "backup" "cache.el"))
         (cache (when (file-exists-p cache-fname)
                  (with-temp-buffer
                    (insert-file-contents cache-fname)
                    (read (current-buffer))))))
    (if (not cache)
        (user-error "No backup cache found")
      (let (orphans)
        (dolist (k (hash-table-keys cache) orphans)
          (unless (file-exists-p (gethash k cache))
            (push `(,(gethash k cache) . ,k) orphans)))
        orphans))))

;;;###autoload
(defun +backups/list-orphaned ()
  "List orphaned backup files."
  (interactive)
  (let ((orphans (+backups--collect-orphans))
        (buf (format "*Backups: Orphans*")))
    (if (not orphans)
        (user-error "No orphans found")
      (get-buffer-create buf)
      (pop-to-buffer buf)
      (erase-buffer)
      (pcase-dolist (`(,orig-fname . ,_key) orphans)
        (let ((backup-files (file-backup-file-names orig-fname)))
          (insert (format "%s \n" orig-fname))
          (dolist (fn backup-files)
            (insert "\t")
            (insert-button
             (format "Version %s" (replace-regexp-in-string ".*~\\([0-9]+\\)~" "\\1" fn))
             'action (lambda (_button) (find-file fn)))
            (insert "\n"))
          (insert "\n")))
      (zenit-view-backup-file-mode +1))))

;;;###autoload
(defun +backups/remove-orphaned ()
  "Remove orphaned backup files."
  (interactive)
  (when (y-or-n-p "Do you want to delete all orphaned backup files?")
    (let* ((cache-fname (file-name-concat zenit-cache-dir "backup" "cache.el"))
           (cache (or (when (file-exists-p cache-fname)
                        (with-temp-buffer
                          (insert-file-contents cache-fname)
                          (read (current-buffer))))
                      (user-error "No backup cache found")))
           (orphans (+backups--collect-orphans)))
      (pcase-dolist (`(,orig-fname . ,key) orphans)
        (let ((backup-files (file-backup-file-names orig-fname)))
          (dolist (bf backup-files)
            (delete-file bf)))
        (remhash key cache))
      (with-temp-file cache-fname
        (erase-buffer)
        (prin1 cache (current-buffer))))))

(provide 'zenit-lib '(backups))
