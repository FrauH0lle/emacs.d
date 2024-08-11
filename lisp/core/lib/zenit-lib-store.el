;; lisp/core/lib/zenit-lib-store.el -*- lexical-binding: t; -*-

;; This little library abstracts the process of writing arbitrary elisp values to a 2-tiered file
;; store (in `zenit-store-dir'/`zenit-store-location').

(eval-when-compile
  (require 'cl-lib))

;; `cl-seq'
(declare-function cl-set-difference "cl-seq")

;; `zenit-lib-files'
(declare-function zenit-files-in "zenit-lib-files")


(defvar zenit-store-dir (concat zenit-data-dir "store/")
  "Directory to look for and store data accessed through this
API.")

(defvar zenit-store-persist-alist ()
  "An alist of alists, containing lists of variables for the
store library to persist across Emacs sessions.")

(defvar zenit-store-location "default"
  "The default location for cache files. This symbol is
translated into a file name under `pcache-directory' (by default
a subdirectory under `zenit-store-dir'). One file may contain
multiple cache entries.")

(defvar zenit--store-table (make-hash-table :test 'equal)
  "Hash table to store key-value pairs for the store library.")

(defun zenit-save-persistent-store-h ()
  "Hook to persist storage when Emacs is killed."
  (let (locations)
    ;; Persist `zenit-store-persist-alist'
    (dolist (alist (butlast zenit-store-persist-alist 1))
      (cl-loop with location = (car alist)
               for var in (cdr alist)
               do (zenit-store-put var (symbol-value var) nil location 'noflush)
               and do (cl-pushnew location locations :test #'equal)))
    ;; Clean up expired entries,
    (dolist (location (zenit-files-in zenit-store-dir :relative-to zenit-store-dir))
      (maphash (lambda (key val)
                 (when (zenit--store-expired-p key val)
                   (cl-pushnew location locations :test #'equal)
                   (zenit-store-rem key location 'noflush)))
               (zenit--store-init location)))
    (mapc #'zenit--store-flush locations)))
(add-hook 'kill-emacs-hook #'zenit-save-persistent-store-h)


;;
;;; Library

;;;###autoload
(defun zenit-store-persist (location variables)
  "Persist VARIABLES (list of symbols) in LOCATION (symbol).
This populates these variables with cached values, if one exists,
and saves them to file when Emacs quits. This cannot persist
buffer-local variables."
  (cl-check-type location string)
  (dolist (var variables)
    (when (zenit-store-member-p var location)
      (set var (zenit-store-get var location))))
  (setf (alist-get location zenit-store-persist-alist nil nil #'equal)
        (append variables (alist-get location zenit-store-persist-alist nil nil #'equal))))

;;;###autoload
(defun zenit-store-desist (location &optional variables)
  "Unregisters VARIABLES (list of symbols) in LOCATION (symbol).
Variables to persist are recorded in
`zenit-store-persist-alist'. Does not affect the actual
variables themselves or their values."
  (cl-check-type location string)
  (if variables
      (setf (alist-get location zenit-store-persist-alist nil nil #'equal)
            (cl-set-difference (cdr (assoc location zenit-store-persist-alist))
                               variables))
    (delq! location zenit-store-persist-alist 'assoc)))

(defun zenit--store-init (&optional location)
  "Initialize a store at LOCATION (defaults to
 `zenit-store-location').

This function retrieves the store data from file if it exists,
otherwise it creates a new hash table and inserts it into
`zenit--store-table'. The store data will be saved to file when
the function `zenit--store-flush' is called."
  (cl-check-type location (or null string))
  (let ((location (or location zenit-store-location)))
    (or (gethash location zenit--store-table)
        (let* ((file-name-handler-alist nil)
               (location-path (expand-file-name location zenit-store-dir)))
          (if (file-exists-p location-path)
              (puthash location
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (setq buffer-file-coding-system 'binary)
                         (insert-file-contents-literally location-path)
                         (read (current-buffer)))
                       zenit--store-table)
            (puthash location (make-hash-table :test 'equal)
                     zenit--store-table))))))

(defun zenit--store-expired-p (key data)
  "Return t if the data stored under KEY is expired.

KEY is an identifier for the data stored in the store.

DATA is a cons cell with the first element being the Time To
Live (TTL) of the data, either as an integer (representing the
number of seconds until expiration), a function that returns t or
nil (representing whether the data is expired), or a
time-stamp (representing the exact time of expiration).

If the TTL is a function, it is called with two arguments: KEY
and DATA. If it returns nil, the data is considered to be
expired.

If the TTL is a time-stamp, the current time is compared against
the time-stamp. If the current time is greater, the data is
considered to be expired.

The function returns t if the data is considered to be expired,
and nil otherwise."
  (let ((ttl (car data)))
    (cond ((functionp ttl)
           (not (funcall ttl key data)))
          ((consp ttl)
           (time-less-p ttl (current-time))))))

(defun zenit--store-flush (location)
  "Write `zenit--store-table' to `zenit-store-dir'."
  (let ((file-name-handler-alist nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (let* ((location (or location zenit-store-location))
           (data (zenit--store-init location)))
      (make-directory zenit-store-dir 'parents)
      (with-temp-file (expand-file-name location zenit-store-dir)
        (prin1 data (current-buffer)))
      data)))


;;;###autoload
(defun zenit-store-get (key &optional location default-value noflush)
  "Retrieve KEY from LOCATION (defaults to `zenit-store-location').
If it doesn't exist or has expired, DEFAULT_VALUE is returned."
  (let ((data (gethash key (zenit--store-init location) default-value)))
    (if (not (or (eq data default-value)
                 (zenit--store-expired-p key data)))
        (cdr data)
      (zenit-store-rem key location noflush)
      default-value)))

;;;###autoload
(defun zenit-store-put (key value &optional ttl location noflush)
  "Set KEY to VALUE in the store at LOCATION.
KEY can be any lisp object that is comparable with `equal'. TTL
is the duration (in seconds) after which this cache entry
expires; if nil, no cache expiration. LOCATION is the super-key
to store this cache item under. It defaults to
`zenit-store-location'."
  (cl-check-type ttl (or null integer function))
  (puthash key (cons (if (integerp ttl)
                         (time-add (current-time) ttl)
                       ttl)
                     value)
           (zenit--store-init location))
  (unless noflush
    (zenit--store-flush location)))

;;;###autoload
(defun zenit-store-rem (key &optional location noflush)
  "Clear a cache LOCATION (defaults to `zenit-store-location')."
  (remhash key (zenit--store-init location))
  (unless noflush
    (zenit--store-flush location)))

;;;###autoload
(defun zenit-store-member-p (key &optional location)
  "Return t if KEY in LOCATION exists.
LOCATION defaults to `zenit-store-location'."
  (let ((nil-value (format "--nilvalue%s--" (current-time))))
    (not (equal (zenit-store-get key location nil-value)
                nil-value))))

;;;###autoload
(defun zenit-store-clear (&optional location)
  "Clear the store at LOCATION (defaults to
`zenit-store-location')."
  (let* ((location (or location zenit-store-location))
         (path (expand-file-name location zenit-store-dir)))
    (remhash location zenit--store-table)
    (when (file-exists-p path)
      (delete-file path)
      t)))

(provide 'zenit-lib '(store))
