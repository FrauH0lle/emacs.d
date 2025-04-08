;; lisp/core/zenit-modules.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `cl-extra'
(declare-function cl-every "cl-extra" (cl-pred cl-seq &rest cl-rest))
(declare-function cl--do-remf "cl-extra" (plist tag))

;; `cl-lib'
(declare-function cl-oddp "cl-lib" (integer))

;; `subr-x'
(declare-function hash-table-keys "subr-x" (hash-table))
;; (autoload #'hash-table-keys "subr-x")

;; `zenit-lib-files'
(declare-function zenit-files-in "zenit-lib-files" (paths &rest rest))


;;
;;; Variables

(defvar zenit-modules (make-hash-table :test #'equal)
  "A hash table of enabled modules.
Set by `zenit-initialize-modules'.")

(defvar zenit-modules-dirs
  (list (expand-file-name "modules/" zenit-local-conf-dir)
        zenit-modules-dir)
  "A list of module root directories. Order determines priority.")

;; Module file variables
(defvar zenit-module-init-file "init.el"
  "The filename for module early initialization config files.

Init files are loaded early, just after the core, and before
modules' config files. They are always loaded, even in
non-interactive sessions, and before
`zenit-before-modules-init-hook'. Related to
`zenit-module-config-file'.")

(defvar zenit-module-config-file "config.el"
  "The filename for module configuration files.

Config files are loaded later, and almost always in interactive
sessions. These run before `zenit-after-modules-config-hook' and
after `zenit-module-init-file'.")

(defvar zenit-module-packages-file "packages.el"
  "The filename for the package configuration file.

They are rarely read in interactive sessions.")

(defvar zenit-module-control-file "control.el"
  "The filename for the module control file.

Control files define dependencies and conflicts, along other
metadata. They are rarely read in interactive sessions.")

(defvar zenit-inhibit-module-warnings (not noninteractive)
  "If non-nil, don't emit deprecated or missing module warnings.
Warnings are emitted at startup.")

;; Custom hooks
(defcustom zenit-before-modules-init-hook nil
  "Hooks run before module init.el files are loaded."
  :group 'zenit
  :type 'hook)

(defcustom zenit-after-modules-init-hook nil
  "Hooks run after module init.el files are loaded."
  :group 'zenit
  :type 'hook)

(defcustom zenit-before-modules-config-hook nil
  "Hooks run before module config.el files are loaded."
  :group 'zenit
  :type 'hook)

(defcustom zenit-after-modules-config-hook nil
  "Hooks run after module config.el files are loaded.
But before the local one."
  :group 'zenit
  :type 'hook)


;;
;;; Module API

(defun zenit-module-p (category module &optional flag)
  "Return t if CATEGORY MODULE and optionally FLAG is enabled.
A module is enabled if it is present in `zenit-modules'."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) zenit-modules)))
    (or (null flag)
        (and (memq flag (plist-get plist :flags))
             t))))

(defun zenit-module-depth (category module &optional initdepth?)
  "Return the depth of CATEGORY MODULE.

If INITDEPTH? is non-nil, use the CAR if a module was given two depths (see
`zenit-module-set')."
  (if-let* ((depth (zenit-module-get category module :depth)))
      (or (if initdepth?
              (car-safe depth)
            (cdr-safe depth))
          depth)
    0))

(defun zenit-module--has-flag-p (flags wanted-flags)
  "Return t if the list of WANTED-FLAGS satisfies the list of FLAGS."
  (declare (pure t) (side-effect-free error-free))
  (cl-loop with flags = (ensure-list flags)
           for flag in (ensure-list wanted-flags)
           for flagstr = (symbol-name flag)
           if (if (eq ?- (aref flagstr 0))
                  (memq (intern (concat "+" (substring flagstr 1)))
                        flags)
                (not (memq flag flags)))
           return nil
           finally return t))

(defun zenit-module-get (category module &optional property)
  "Return the plist for CATEGORY MODULE.
Gets PROPERTY,specifically, if set."
  (declare (pure t) (side-effect-free t))
  (when-let* ((plist (gethash (cons category module) zenit-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun zenit-module-put (category module &rest plist)
  "Set a PROPERTY for CATEGORY MODULE to VALUE.
PLIST should be additional pairs of PROPERTY and VALUEs.

\(fn CATEGORY MODULE PROPERTY VALUE &rest [PROPERTY VALUE [...]])"
  (puthash (cons category module)
           (if-let* ((old-plist (zenit-module-get category module)))
               (if (null plist)
                   old-plist
                 (when (cl-oddp (length plist))
                   (signal 'wrong-number-of-arguments (list (length plist))))
                 (while plist
                   (plist-put old-plist (pop plist) (pop plist)))
                 old-plist)
             plist)
           zenit-modules))


(defun zenit-module-set (category module &rest plist)
  "Enables a module by adding it to `zenit-modules'.

CATEGORY is a keyword, MODULE is a symbol, PLIST is a plist that
accepts the following properties:

  :path STRING
    Path to the directory where this module lives.
  :depth INT|(INITDEPTH . CONFIGDEPTH)
    Determines module load order. If a cons cell, INITDEPTH
    determines the load order of the module's init.el, while
    CONFIGDEPTH determines the same for all other config
    files (config.el, packages.el, doctor.el, etc).
  :flags (SYMBOL...)
    A list of activated flags for this module.

If PLIST consists of a single nil, the module is purged from
memory instead."
  (if (car plist)
      (let* ((depth (ensure-list (or (plist-get plist :depth) 0)))
             (idepth (or (cdr depth) (car depth)))
             (cdepth (car depth))
             (idx (hash-table-count zenit-modules)))
        ;; We cache the module index, flags, and features in symbol plists for
        ;; fast lookups in `modulep!' and elsewhere. plists are lighter and
        ;; faster than hash tables for datasets this size, and this information
        ;; is looked up *very* often.
        (put category module
             (vector idx idepth cdepth
                     category module
                     (plist-get plist :flags)
                     (plist-get plist :features)))
        ;; The hash table will always been the formal storage for modules.
        (puthash (cons category module) plist zenit-modules))
    (remhash (cons category module) zenit-modules)
    (cl-remf (symbol-plist category) module)))


(defun zenit-module-list (&optional paths-or-all initorder?)
  "Return a list of (:group . name) module keys in order of their :depth.

PATHS-OR-ALL can either be a non-nil value or a list of
directories. If given a list of directories, return a list of
module keys for all modules present underneath it. If non-nil,
return the same, but search `zenit-modules-dirs' (includes :core
and :local-conf). Modules that are enabled are sorted first by
their :depth, followed by disabled modules in lexicographical
order.

If INITORDER? is non-nil, sort modules by their initdepth, rather
than their configdepth. See `zenit-module-set' for details."
  (sort (if paths-or-all
            (delete-dups
             (append (seq-remove #'cdr (zenit-module-list nil initorder?))
                     (zenit-files-in (if (listp paths-or-all)
                                         paths-or-all
                                       zenit-modules-dirs)
                                     :map #'zenit-module-from-path
                                     :type 'dirs
                                     :mindepth 1
                                     :depth 1)))
          (hash-table-keys zenit-modules))
        (let ((idx (if initorder? 1 2)))
          (lambda! ((groupa . namea) (groupb . nameb))
            (let ((a (get groupa namea))
                  (b (get groupb nameb)))
              (or (null b)
                  (and
                   a (let ((adepth (aref a idx))
                           (bdepth (aref b idx)))
                       (if (= adepth bdepth)
                           (< (aref a 0) (aref b 0))
                         (< adepth bdepth))))))))))

(defun zenit-module-expand-path (category module &optional file)
  "Expands a path to FILE relative to CATEGORY and MODULE.

CATEGORY is a keyword. MODULE is a symbol. FILE is an optional
string path. If the category isn't enabled this returns nil. For
finding disabled modules use `zenit-module-locate-path'."
  (when-let* ((path (zenit-module-get category module :path)))
    (if file
        (file-name-concat path file)
      path)))

(defun zenit-module-locate-path (category &optional module file)
  "Search `zenit-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g.
\\='python). FILE is a string that will be appended to the
resulting path. If no path exists, this returns nil, otherwise an
absolute path."
  (let (file-name-handler-alist)
    (if-let* ((path (zenit-module-expand-path category module file)))
        (if (or (null file)
                (file-exists-p path))
            path)
      (let* ((category (zenit-keyword-name category))
             (module (if module (symbol-name module)))
             (path (file-name-concat category module file)))
        (if file
            ;; `locate-file-internal' is a little faster for finding files, but
            ;; its interface for finding directories is clumsy.
            (locate-file-internal path zenit-modules-dirs '("" ".elc" ".el"))
          (cl-loop for default-directory in zenit-modules-dirs
                   if (file-exists-p path)
                   return (expand-file-name path)))))))

(defun zenit-module-locate-paths (module-list file)
  "Return all existing paths to FILE under each module in MODULE-LIST.

MODULE-LIST is a list of cons cells (GROUP . NAME). See
`zenit-module-list' for an example."
  (cl-loop for (group . name) in (or module-list (zenit-module-list))
           if (zenit-module-locate-path group name file)
           collect it))

(defun zenit-module-from-path (path &optional enabled-only)
  "Return a cons cell (CATEGORY . MODULE) derived from PATH (a file path).
If ENABLED-ONLY, return nil if the containing module isn't
enabled."
  (let* ((file-name-handler-alist nil)
         (path (expand-file-name path)))
    (save-match-data
      (cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
             (when-let* ((category (zenit-keyword-intern (match-string 1 path)))
                         (module   (intern (match-string 2 path))))
               (and (or (null enabled-only)
                        (zenit-module-p category module))
                    (cons category module))))
            ((file-in-directory-p path zenit-core-dir)
             (cons :core nil))
            ((file-in-directory-p path zenit-local-conf-dir)
             (cons :local-conf nil))))))

(defun zenit-module-load-path (&optional module-dirs)
  "Return a list of file paths to activated modules.

The list is in no particular order and its file paths are
absolute. If MODULE-DIRS is non-nil, include all modules (even
disabled ones) available in those directories."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (cat . mod) in (zenit-module-list module-dirs)
           collect (zenit-module-locate-path cat mod)))

(defun zenit-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST."
  (let ((mplist (copy-sequence mplist))
        (inhibit-message zenit-inhibit-module-warnings)
        results
        category m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
             (setq category m))
            ((null category)
             (error "No module category specified for %s" m))
            ((and (listp m) (keywordp (car m)))
             (pcase (car m)
               (:cond
                (cl-loop for (cond . mods) in (cdr m)
                         if (eval cond t)
                         return (prependq! mplist mods)))
               (:if (if (eval (cadr m) t)
                        (push (caddr m) mplist)
                      (prependq! mplist (cdddr m))))
               (test (if (xor (eval (cadr m) t)
                              (eq test :unless))
                         (prependq! mplist (cddr m))))))
            ((catch 'zenit-modules
               ;; (let ((module (if (listp m) (car m) m)))
               (let* ((module (if (listp m) (car m) m))
                      (flags  (if (listp m) (cdr m))))
                 (push (funcall fn category module :flags flags)
                       results))))))
    (when noninteractive
      (setq zenit-inhibit-module-warnings t))
    (nreverse results)))

(defvar zenit--module-dependencies nil
  "Stores module dependency messages.")
(defvar zenit--module-conflicts nil
  "Stores module conflict messages.")

(defun zenit-module-resolve (module &optional conflicts)
  "Processes a MODULE's `zenit-module-control-file'.

If CONFLICTS is nil, the function will resolve dependencies and
add them to `zenit-modules' if not already set. Otherwise it look
for conflicts and add them to `zenit--module-conflicts'."
  (cl-destructuring-bind (category . module) module
    (let* ((path (zenit-module-locate-path category module))
           (control-file (file-name-concat path zenit-module-control-file)))
      (when (file-exists-p control-file)
        (let ((control
               (with-temp-buffer
                 (insert-file-contents-literally control-file)
                 (read (current-buffer))))
              (key (if conflicts :conflicts :depends)))
          (dolist (entry (plist-get control key))
            (cl-destructuring-bind (flag dep-category dep-module &rest dep-flags) entry
              ;; flag is t (module itself) or a module flag
              (when (and (if (not (eq flag t))
                             (zenit-module-p category module flag)
                           (setq flag nil)
                           (zenit-module-p category module))
                         ;; Check if module is already set as desired. Also take
                         ;; into account that there can be multiple dependency
                         ;; flags.
                         (if dep-flags
                             (let (result)
                               (dolist (flag dep-flags result)
                                 (if (zenit-module-p dep-category dep-module flag)
                                     (push t result)
                                   (push nil result)))
                               ;; For :depends we want to proceed only if a
                               ;; module is not set up and the opposite in case
                               ;; of :conflicts
                               (if (eq key :depends)
                                   (not (cl-every (lambda (x) (eq x t)) result))
                                 (cl-every (lambda (x) (eq x t)) result)))
                           (if (eq key :depends)
                               (not (zenit-module-p dep-category dep-module))
                             (zenit-module-p dep-category dep-module))))
                (cond ((eq key :depends)
                       (if (zenit-module-get dep-category dep-module)
                           (let ((old-flags (zenit-module-get
                                             dep-category dep-module :flags)))
                             (push (format "Adding %s %s %s as dependency of %s %s %s"
                                           dep-category dep-module dep-flags category module flag)
                                   zenit--module-dependencies)
                             ;; Append new flags to existing module
                             (zenit-module-set
                              dep-category dep-module
                              :path (zenit-module-locate-path dep-category dep-module)
                              :flags (append old-flags dep-flags)))
                         ;; If module is not set yet, add it
                         (push (format "Adding %s %s %s as dependency of %s %s"
                                       dep-category dep-module dep-flags category module)
                               zenit--module-dependencies)
                         (zenit-module-set
                          dep-category dep-module
                          :path (zenit-module-locate-path dep-category dep-module)
                          :flags dep-flags)))
                      ((eq key :conflicts)
                       (push (format"%s %s %s conflicts with %s %s %s"
                                    category module flag dep-category dep-module dep-flags)
                             zenit--module-conflicts)))))))))))


;;
;;; Module config macros

(put :if     'lisp-indent-function 2)
(put :when   'lisp-indent-function 'defun)
(put :unless 'lisp-indent-function 'defun)

(defmacro modules! (&rest modules)
  "Bootstraps Emacs and its modules.

If the first item in MODULES doesn't satisfy `keywordp', MODULES
is evaluated, otherwise, MODULES is a multiple-property list (a
plist where each key can have multiple, linear values).

The bootstrap process involves making sure the essential
directories exist, core packages are installed,
`zenit-config-init-file' is loaded, `zenit-packages-file' cache
exists (and is loaded) and, finally, loads your private
init.el (which should contain your `modules!' block).

Module load order is determined by your `modules!' block. See
`zenit-modules-dirs' for a list of all recognized module trees.
Order defines precedence (from most to least)."
  `(when noninteractive
     (zenit-module-mplist-map
      (lambda (category module &rest plist)
        (let ((path (zenit-module-locate-path category module)))
          (unless path
            (print! (warn "Failed to locate a '%s %s' module") category module))
          (apply #'zenit-module-set category module
                 :path path
                 plist)))
      ,@(if (keywordp (car modules))
            (list (list 'quote modules))
          modules))
     ;; Add dependencies
     (zenit-log "modules: Resolving module dependencies")
     (dolist (module (zenit-module-list))
       (zenit-module-resolve module))
     (dolist (msg zenit--module-dependencies)
       (zenit-log "modules: %s" msg))
     (zenit-log "modules: Dependencies resolved")
     ;; Check for conflicts
     (dolist (module (zenit-module-list))
       (zenit-module-resolve module 'conflicts))
     (when zenit--module-conflicts
       (print! (error "Module conflicts detected"))
       (print-group!
         (dolist (msg zenit--module-conflicts)
           (print! (item "%s" msg))))
       (signal 'zenit-module-error (list "Module conflicts detected")))
     zenit-modules))


;;
;;; Module context

(defvar zenit--empty-module-context [nil nil nil nil nil nil nil]
  "Empty module context.
This vector is used as a placeholder and represents a context
where no module specifics have been defined.")

(eval-and-compile
  (put 'zenit-module-context 'keys '(:index 0 :initdepth 1 :configdepth 2
                                     :group 3 :name 4 :flags 5 :features 6)))
(defvar zenit-module-context zenit--empty-module-context
  "A vector describing the module associated it with the active context.
Contains the following: [INDEX INITDEPTH CONFIGDEPTH :GROUP
MODULE FLAGS FEATURES] Do not directly set this variable, only
let-bind it.")

(defmacro zenit-module--context-field (field)
  "Retrieve the index of FIELD from the `zenit-module-context'.

Note that FIELD is evaluated at macro-expansion time, and it
should be a valid symbol that matches a key within the
`zenit-module-context' property list."
  (plist-get (get 'zenit-module-context 'keys) field))

(defun zenit-module-context-get (field &optional context)
  "Return the FIELD of CONTEXT.

FIELD should be one of `index', `initdepth', `configdepth',
`group', `name', `flags', or `features'. CONTEXT should be a
`zenit-module-context' vector. If omitted, defaults to
`zenit-module-context'."
  (aref (or context zenit-module-context)
        (plist-get (get 'zenit-module-context 'keys)
                   field)))

(defun zenit-module-context (group &optional name)
  "Create a `zenit-module-context' from a module by GROUP and NAME.
If NAME is omitted, GROUP is treated as a module key cons
cell: (GROUP . NAME)."
  (declare (side-effect-free t))
  (let ((key (if name (cons group name) group)))
    (or (get (or (car-safe key) key)
             (cdr-safe key))
        zenit--empty-module-context)))

(defun zenit-module-context-key (&optional context)
  "Return the module of the active `zenit-module-context' as a
module key."
  (declare (side-effect-free t))
  (let ((context (or context zenit-module-context)))
    (cons (aref context (zenit-module--context-field :group))
          (aref context (zenit-module--context-field :name)))))

(defmacro zenit-module-context-with (module-key &rest body)
  "Evaluate BODY with `zenit-module-context' informed by MODULE-KEY."
  (declare (indent 1))
  `(let ((zenit-module-context
          (let ((module-key ,module-key))
            (zenit-module-context module-key))))
     (zenit-log ":context:module: =%s" zenit-module-context)
     ,@body))

(defmacro modulep! (category &optional module &rest flags)
  "Return t if :CATEGORY MODULE (and +FLAGS) are enabled.

If FLAGS is provided, returns t if CATEGORY MODULE has all of
FLAGS enabled.

  (modulep! :config default +flag)
  (modulep! :config default +flag1 +flag2 +flag3)

CATEGORY and MODULE may be omitted when this macro is used from a
module's source. Like so:

  (modulep! +flag3 +flag1 +flag2)
  (modulep! +flag)

FLAGS can be negated. E.g. This will return non-nil if ':tools
lsp' is enabled without `+eglot':

  (modulep! :tools lsp -eglot)

To interpolate dynamic values, use comma:

  (let ((flag '-eglot))
    (modulep! :tools lsp ,flag))

For more about modules and flags, see `modules!'."
  ;; This macro bypasses the module API to spare startup their runtime cost, as
  ;; `modulep!' gets called *a lot* during startup.
  (if (keywordp category)
      (let ((ctxtform `(get (backquote ,category) (backquote ,module))))
        (if flags
            `(when-let* ((ctxt ,ctxtform))
               (zenit-module--has-flag-p
                (aref (or ctxt zenit--empty-module-context)
                      (zenit-module--context-field :flags))
                (backquote ,flags)))
          `(and ,ctxtform t)))
    (let ((flags (delq nil (cons category (cons module flags)))))
      (if (aref zenit-module-context 0)
          `(zenit-module--has-flag-p
            ',(aref zenit-module-context
                    (zenit-module--context-field :flags))
            (backquote ,flags))
        `(let ((file (file!)))
           (if-let* ((module (zenit-module-from-path file)))
               (zenit-module--has-flag-p
                (aref (or (get (car module) (cdr module))
                          zenit--empty-module-context)
                      (zenit-module--context-field :flags))
                (backquote ,flags))
             (error "(modulep! %s) couldn't resolve current module from %s"
                    (backquote ,flags) (abbreviate-file-name file))))))))


;;
;;; Defaults

;; Register zenit's two virtual module categories, representing zenit's core and
;; the user's config; which are always enabled.
(zenit-module-set :core nil :path zenit-core-dir :depth -110)
(zenit-module-set :local-conf nil :path zenit-local-conf-dir :depth '(-105 . 105))

(provide 'zenit-modules)
