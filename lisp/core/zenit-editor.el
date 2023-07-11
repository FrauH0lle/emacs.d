;; lisp/core/zenit-editor.el -*- lexical-binding: t; -*-


(defvar zenit-detect-indentation-excluded-modes
  '(fundamental-mode pascal-mode so-long-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(defvar-local zenit-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should
try to detect indentation settings or not. This should be set by
editorconfig if it successfully sets indent_style/indent_size.")

(defvar zenit-inhibit-large-file-detection nil
  "If non-nil, inhibit large/long file detection when opening
files.")

(defvar zenit-large-file-p nil)
(put 'zenit-large-file-p 'permanent-local t)

(defvar zenit-large-file-size-alist '(("." . 1.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.

If a file is opened and discovered to be larger than the
threshold, we perform emergency optimizations to prevent Emacs
from hanging, crashing or becoming unusably slow.

These thresholds are in MB, and is used by
`zenit--prepare-for-large-files-a'.")

(defvar zenit-large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `zenit-optimize-for-large-files-h' will
ignore.")


;;
;;; File handling

(defadvice! zenit--prepare-for-large-files-a (size _ filename &rest _)
  "Sets `zenit-large-file-p' if the file is considered large.

Uses `zenit-large-file-size-alist' to determine when a file is
too large. When `zenit-large-file-p' is set, other plugins can
detect this and reduce their runtime costs (or disable
themselves) to ensure the buffer is as fast as possible."
  :before #'abort-if-file-too-large
  (and (numberp size)
       (null zenit-inhibit-large-file-detection)
       (ignore-errors
         (> size
            (* 1024 1024
               (assoc-default filename zenit-large-file-size-alist
                              #'string-match-p))))
       (setq-local zenit-large-file-p size)))

(defhook! zenit-optimize-for-large-files-h ()
  "Trigger `so-long-minor-mode' if the file is large."
  'find-file-hook
  (when (and zenit-large-file-p buffer-file-name)
    (if (or zenit-inhibit-large-file-detection
            (memq major-mode zenit-large-file-excluded-modes))
        (kill-local-variable 'zenit-large-file-p)
      (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
        (so-long-minor-mode +1))
      (message "Large file detected! Cutting a few corners to improve performance..."))))


;; Usually we do not want to follow symlinks, as it changes `default-directory'
;; and the context we working in. Use `zenit/toggle-symlink' to change between
;; the link and the source. However, if the symlink points towards a
;; version-controlled file, we usually do want to follow the link.
(setq vc-follow-symlinks t)
;; When we do that, we record where we are coming from, so we can still toggle
;; between the link and the source.
(defvar-local zenit--symlink-origin nil)
(defadvice! zenit--record-symlink-origin-a (orig-fn)
  "Save the origin after following a symlink."
  :around #'vc-follow-link
  (let ((origin (buffer-file-name)))
    (funcall orig-fn)
    (setq-local zenit--symlink-origin origin)))

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(defhook! zenit-create-missing-directories-h ()
  "Automatically create missing directories when creating new files."
  'find-file-not-found-functions
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))

;; Backups and lockfiles
(setq make-backup-files t ; Activate backups
      ;; If we backup, backup (nearly) everything
      vc-make-backup-files t
      create-lockfiles nil
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat zenit-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; Turn on auto-save, so we have a fallback in case of crashes or lost data. Use
;; `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `zenit-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat zenit-cache-dir "autosave/")
      tramp-auto-save-directory  (concat zenit-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(defhook! zenit-guess-mode-h ()
  "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A
shebang line or file path may exist now."
  'after-save-hook
  (when (eq major-mode 'fundamental-mode)
    (let ((buffer (or (buffer-base-buffer) (current-buffer))))
      (and (buffer-file-name buffer)
           (eq buffer (window-buffer (selected-window))) ; only visible buffers
           (set-auto-mode)))))

(defadvice! zenit--shut-up-autosave-a (fn &rest args)
  "If a file has autosaved data, `after-find-file' will pause for 1
second to tell you about it. Very annoying. This prevents that."
  :around #'after-find-file
  (letf! ((#'sit-for #'ignore))
    (apply fn args)))

;; HACK Emacs generates long file paths for its auto-save files; long =
;;      `auto-save-list-file-prefix' + `buffer-file-name'. If too long, this
;;      will murder the filesystem. Thus, we compress `buffer-file-name' to a
;;      stable 40 characters via `sha1'.
(defadvice! zenit-make-hashed-auto-save-file-name-a (fn)
  "Compress the auto-save file name so paths don't get too long."
  :around #'make-auto-save-file-name
  (let ((buffer-file-name
         (if (or
              ;; Don't do anything for non-file-visiting buffers. Names
              ;; generated for those are short enough already.
              (null buffer-file-name)
              ;; If an alternate handler exists for this path, bow out. Most of
              ;; them end up calling `make-auto-save-file-name' again anyway, so
              ;; we still achieve this advice's ultimate goal.
              (find-file-name-handler buffer-file-name
                                      'make-auto-save-file-name))
             buffer-file-name
           (sha1 buffer-file-name))))
    (funcall fn)))

;; HACK ...does the same for Emacs backup files, but also packages that use
;;      `make-backup-file-name-1' directly (like undo-tree).
(defadvice! zenit-make-hashed-backup-file-name-a (fn file)
  "A few places use the backup file name so paths don't get too
long."
  :around #'make-backup-file-name-1
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (car alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil)
          (setq alist (cdr alist)))))
    (let ((file (funcall fn file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))

;; HACK In order to make the hashed file names compatible with
;;      `zenit-file-backup-mode', we store the hashed filenames together with
;;      the original file names in a hash table. We can use that information to
;;      check if a backup file has been orphaned.
(defadvice! zenit-cache-hashed-backup-file-name-a (fn &rest _)
  "Let `backup-buffer' also create the cache entry for the backup
system."
  :around #'backup-buffer
  (let ((return (funcall fn)))
    (when (buffer-local-value buffer-backed-up (current-buffer))
      (let* ((cache-fname (file-name-concat zenit-cache-dir "backup" "cache.el"))
             (cache (or (when (file-exists-p cache-fname)
                          (with-temp-buffer
                            (insert-file-contents cache-fname)
                            (read (current-buffer))))
                        (make-hash-table :test 'equal)))
             (fname (file-chase-links buffer-file-name))
             (backup-fname (make-backup-file-name-1 fname))
             (sha (string-remove-suffix "~" (file-name-nondirectory backup-fname))))
        (puthash sha (expand-file-name fname) cache)
        (with-temp-file cache-fname
          (erase-buffer)
          (prin1 cache (current-buffer)))))
    return))


;;
;;; Formatting

;; Favor spaces over tabs.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Always indent.
(setq-default tab-always-indent t)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Still good
(setq-default fill-column 80)

;; Continue wrapped words at whitespace, rather than in the middle of a word.
(setq-default word-wrap t)
;; ...but don't do any wrapping by default. It's expensive. Enable
;; `visual-line-mode' if you want soft line-wrapping. `auto-fill-mode' for hard
;; line-wrapping.
(setq-default truncate-lines t)
;; If enabled (and `truncate-lines' was disabled), soft wrapping no longer
;; occurs when that window is less than `truncate-partial-width-windows'
;; characters wide. We don't need this, and it's extra work for Emacs otherwise,
;; so off it goes.
(setq truncate-partial-width-windows nil)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)


;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)


;;
;;; Extra file extensions to support

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))


;;
;;; Built-in plugins

(use-package! autorevert
  ;; revert buffers when their files/state have changed
  :hook (after-save . zenit-auto-revert-buffers-h)
  :hook (zenit-switch-buffer . zenit-auto-revert-buffer-h)
  :hook (zenit-switch-window . zenit-auto-revert-buffer-h)
  :init
  (add-function :after after-focus-change-function #'zenit-auto-revert-buffers-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of file watchers _or_ aggressively poll your buffer list every X
  ;; seconds. Too many watchers can grind Emacs to a halt if you preform
  ;; expensive or batch processes on files outside of Emacs (e.g. their mtime
  ;; changes), and polling your buffer list is terribly inefficient as your
  ;; buffer list grows into the hundreds.
  ;;
  ;; We do this lazily instead. i.e. All visible buffers are reverted
  ;; immediately when a) a file is saved or b) Emacs is refocused (after using
  ;; another app). Meanwhile, buried buffers are reverted only when they are
  ;; switched to. This way, Emacs only ever has to operate on, at minimum, a
  ;; single buffer and, at maximum, ~10 buffers (after all, when do you ever
  ;; have more than 10 windows in any single frame?).
  (defun zenit-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun zenit-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (zenit-visible-buffers))
      (with-current-buffer buf
        (zenit-auto-revert-buffer-h)))))


;;;###package bookmark
(setq bookmark-default-file (concat zenit-data-dir "bookmarks"))


(use-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally easymenu tree-widget timer
  :hook (zenit-first-file . recentf-mode)
  :commands recentf-open-files
  :custom (recentf-save-file (concat zenit-cache-dir "recentf"))
  :config
  (setq recentf-auto-cleanup nil     ; Don't. We'll auto-cleanup on shutdown
        recentf-max-saved-items 200) ; default is 20

  (defun zenit--recentf-file-truename-fn (file)
    "Return the true name of FILE if it is a local or sudo remote
 file; otherwise, return FILE as it is. For the true name,
 abbreviations for the home directory are used."
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
      file))

  ;; Anything in runtime folders
  (add-to-list 'recentf-exclude
               (concat "^" (regexp-quote (or (getenv "XDG_RUNTIME_DIR")
                                             "/run"))))

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'zenit--recentf-file-truename-fn)

  ;; Text properties inflate the size of recentf's files, and there is no
  ;; purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (defhook! zenit--recentf-touch-buffer-h ()
    "Bump file in recent file list when it is switched or written to."
    '(zenit-switch-window-hook write-file-functions)
    (when buffer-file-name
      (recentf-add-file buffer-file-name))
    ;; Return nil for `write-file-functions'
    nil)

  (defhook! zenit--recentf-add-dired-directory-h ()
    "Add dired directories to recentf file list."
    'dired-mode-hook
    (recentf-add-file default-directory))

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300))
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'recentf-cleanup))

  ;; Otherwise `load-file' calls in `recentf-load-list' pollute *Messages*
  (advice-add #'recentf-load-list :around #'zenit-shut-up-a))


(use-package! savehist
  ;; persist variables across sessions
  :defer-incrementally custom
  :hook (zenit-first-input . savehist-mode)
  :custom (savehist-file (concat zenit-cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches

  (defhook! zenit-savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache
size."
    'savehist-save-hook
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))
          register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item))))

  (defhook! zenit-savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window
configurations). Otherwise, `savehist' would discard
`register-alist' entirely if we don't omit the unwritable
tidbits."
    'savehist-save-hook
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
                (cl-remove-if-not #'savehist-printable register-alist))))


(use-package! saveplace
  ;; persistent point location in buffers
  :hook (zenit-first-file . save-place-mode)
  :custom (save-place-file (concat zenit-cache-dir "saveplace"))
  :config
  (defadvice! zenit--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! zenit--inhibit-saveplace-in-long-files-a (fn &rest args)
    :around #'save-place-to-alist
    (unless zenit-large-file-p
      (apply fn args)))

  (defadvice! zenit--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to
prettify cache files, so this replace calls to `pp' with the much
faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn))))


(use-package! server
  :when (display-graphic-p)
  :after-call zenit-first-input-hook zenit-first-file-hook after-focus-change-function ; focus-out-hook
  :defer 1
  :config
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))


(after! tramp
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-ignore-dir-regexp (format "%s\\|%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp
                                     "[/\\\\]node_modules")))


;;
;;; Packages

(use-package! better-jumper
  :hook (zenit-first-input . better-jumper-mode)
  :commands zenit-set-jump-a zenit-set-jump-maybe-a zenit-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config
  (defun zenit-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun zenit-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun zenit-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for
short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'zenit-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'zenit-set-jump-a))


(use-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `zenit-detect-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . zenit-detect-indentation-h)
  :config
  (defun zenit-detect-indentation-h ()
    "Detect and set the appropriate indentation style for the
current buffer. "
    (unless (or (not after-init-time)
                zenit-inhibit-indent-detection
                zenit-large-file-p
                (memq major-mode zenit-detect-indentation-excluded-modes)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! zenit--fix-broken-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their
indentation, like `nim-mode'. This prevents them from leaving
Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg)))))

(use-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

  (defun zenit-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; Standard location for the Emacs source code
  (setq source-directory (file-name-concat zenit-data-dir "src/"))

  ;; This is initialized to nil by `find-func' if the source is not cloned when
  ;; the library is loaded
  (setq find-function-C-source-directory
        (expand-file-name "src" source-directory))

  (defun zenit--clone-emacs-source-maybe ()
    "Prompt user to clone Emacs source repository if needed."
    (when (and (not (file-directory-p source-directory))
               (not (get-buffer "*clone-emacs-src*"))
               (yes-or-no-p "Clone Emacs source repository? "))
      (make-directory (file-name-directory source-directory) 'parents)
      (let ((branch (concat "emacs-" (prin1-to-string emacs-major-version)))
            (compilation-buffer-name-function
             (lambda (&rest _)
               "*clone-emacs-src*")))
        (save-current-buffer
          (compile
           (format
            "git clone -b %s --depth 1 https://github.com/emacs-mirror/emacs.git %s"
            (shell-quote-argument branch)
            (shell-quote-argument source-directory)))))))

  (after! find-func
    (defadvice! +find-func--clone-emacs-source-a (&rest _)
      "Clone Emacs source if needed to view definition."
      :before #'find-function-C-source
      (zenit--clone-emacs-source-maybe)))

  :config
  (defadvice! +helpful--clone-emacs-source-a (library-name)
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    :before #'helpful--library-path
    (when (member (file-name-extension library-name) '("c" "rs"))
      (zenit--clone-emacs-source-maybe))))


;;;###package imenu
(add-hook 'imenu-after-jump-hook #'recenter)


(use-package! smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :hook (zenit-first-buffer . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  (add-to-list 'zenit-point-in-string-functions 'sp-point-in-string)
  (add-to-list 'zenit-point-in-comment-functions 'sp-point-in-comment)
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil)
    ;; Smartparens conditional binds a key to C-g when sp overlays are active
    ;; (even if they're invisible). This disruptively changes the behavior of
    ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
    ;; I don't see the point of this keybind, so...
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (defhook! zenit-init-smartparens-in-eval-expression-h ()
    "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
    'eval-expression-minibuffer-setup-hook
    (when smartparens-global-mode (smartparens-mode +1)))

  (defhook! zenit-init-smartparens-in-minibuffer-maybe-h ()
    "Enable `smartparens' for non-`eval-expression' commands.
Only enable `smartparens-mode' if `smartparens-global-mode' is
on."
    'minibuffer-setup-hook
    (when (and smartparens-global-mode (memq this-command '(evil-ex)))
      (smartparens-mode +1)))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar zenit-buffer-smartparens-mode nil
    "Variable indicating whether smartparens mode is active in the
current buffer.")
  (defhook! zenit-enable-smartparens-mode-maybe-h ()
    'evil-replace-state-exit-hook
    (when zenit-buffer-smartparens-mode
      (turn-on-smartparens-mode)
      (kill-local-variable 'zenit-buffer-smartparens-mode)))
  (defhook! zenit-disable-smartparens-mode-maybe-h ()
    'evil-replace-state-entry-hook
    (when smartparens-mode
      (setq-local zenit-buffer-smartparens-mode t)
      (turn-off-smartparens-mode))))


(use-package! so-long
  :hook (zenit-first-file . global-so-long-mode)
  :config
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode)))


(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (zenit-first-buffer . ws-butler-global-mode)
  :config
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (setq ws-butler-keep-whitespace-before-point nil))

(provide 'zenit-editor)
