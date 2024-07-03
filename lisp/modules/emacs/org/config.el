;; lang/org/config.el -*- lexical-binding: t; -*-


(defvar +org-babel-native-async-langs '(python)
  "Languages that will use `ob-comint' instead of `ob-async'
for `:async'.")

(defvar +org-babel-mode-alist
  '((c . C)
    (cpp . C)
    (C++ . C)
    (D . C)
    (elisp . emacs-lisp)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (rust . rustic-babel)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary
for babel libraries (ob-*.el) that don't match the name of the
language.

For example, (fish . shell) will cause #+begin_src fish blocks to
load ob-shell.el when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing
src block. They take one argument (the language specified in the
src block, as a string). Stops at the first function to return
non-nil.")

(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in
the default `org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in
the default `org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that
do not specify a target file.

Is relative to `org-directory', unless it is absolute. Is used in
the default `org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in
the default `org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph")

(defvar +org-habit-min-width 30
  "Hides the consistency graph if the `org-habit-graph-column' is
 less than this value")

(defvar +org-habit-graph-window-ratio 0.3
  "The ratio of the consistency graphs relative to the window
width")


;;
;;; `org-load' hooks

(defun +org-init-org-directory-h ()
  (unless org-directory
    (setq-default org-directory "~/org"))
  (unless org-id-locations-file
    (setq org-id-locations-file (expand-file-name ".orgids" org-directory))))


(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        ;; `showeverything' is org's default, but it doesn't respect
        ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
        ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
        ;; respects these settings.
        org-startup-folded nil)

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first
        ;; level of each outline candidates. i.e. all the candidates under the
        ;; "Tasks" heading are just "Tasks/". This is unhelpful. We want the
        ;; full path to each refile target! e.g. FILE/Task/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Larger previews
  (plist-put org-format-latex-options :scale 1.5)

  ;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
  ;;      underlying faces like the `org-todo' face does, so we define our own
  ;;      intermediary faces that extend from org-todo.
  (with-no-warnings
    (custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  ;; Automatic indent detection in org files is meaningless
  (add-to-list 'zenit-detect-indentation-excluded-modes 'org-mode)

  ;; (set-ligatures! 'org-mode
  ;;                 :name "#+NAME:"
  ;;                 :name "#+name:"
  ;;                 :src_block "#+BEGIN_SRC"
  ;;                 :src_block "#+begin_src"
  ;;                 :src_block_end "#+END_SRC"
  ;;                 :src_block_end "#+end_src"
  ;;                 :quote "#+BEGIN_QUOTE"
  ;;                 :quote "#+begin_quote"
  ;;                 :quote_end "#+END_QUOTE"
  ;;                 :quote_end "#+end_quote")
  )


(defun +org-init-babel-h ()
  (setq org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-src-window-setup 'other-window)

  ;; I prefer C-c C-c over C-c ' (more consistent)
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; so `org-src-mode' buffers can be switched to
  ;; (add-hook 'org-src-mode-hook #'+workspaces-add-current-buffer-h)
  (defun +org-src-buffer-p (buf)
    (with-current-buffer buf
      (bound-and-true-p org-src-mode)))
  (add-to-list 'zenit-real-buffer-functions #'+org-src-buffer-p nil #'eq)

  ;; Don't process babel results asynchronously when exporting org, as they
  ;; won't likely complete in time, and will instead output an ob-async hash
  ;; instead of the wanted evaluation results.
  (after! ob
    (add-to-list 'org-babel-default-lob-header-args '(:sync)))

  (defadvice! +org-babel-disable-async-maybe-a (orig-fn &optional fn arg info params)
    "Use ob-comint where supported, disable async altogether
 where it isn't.

We have access to two async backends: ob-comint or ob-async,
which have different requirements. This advice tries to pick the
best option between them, falling back to synchronous execution
otherwise. Without this advice, they die with an error; terrible
UX!

Note: ob-comint support will only kick in for languages listed in
`+org-babel-native-async-langs'.

Also adds support for a `:sync' parameter to override `:async'."
    :around #'ob-async-org-babel-execute-src-block
    (if (null fn)
        (funcall orig-fn fn arg info params)
      (let* ((info (or info (org-babel-get-src-block-info)))
             (params (org-babel-merge-params (nth 2 info) params)))
        (if (or (assq :sync params)
                (not (assq :async params))
                (member (car info) ob-async-no-async-languages-alist)
                ;; ob-comint requires a :session, ob-async does not, so fall
                ;; back to ob-async if no :session is provided.
                (unless (member (alist-get :session params) '("none" nil))
                  (unless (memq (let* ((lang (nth 0 info))
                                       (lang (cond ((symbolp lang) lang)
                                                   ((stringp lang) (intern lang)))))
                                  (or (alist-get lang +org-babel-mode-alist)
                                      lang))
                                +org-babel-native-async-langs)
                    (message "Org babel: %s :session is incompatible with :async. Executing synchronously!"
                             (car info))
                    (sleep-for 0.2))
                  t))
            (funcall fn arg info params)
          (funcall orig-fn fn arg info params)))))

  ;; HACK Seems `org-babel-do-in-edit-buffer' has the side effect of deleting
  ;;   side windows. This advice suppresses this behavior wherever it is known
  ;;   to be used.
  (defadvice! +org-fix-window-excursions-a (fn &rest args)
    "Suppress changes to the window config anywhere
`org-babel-do-in-edit-buffer' is used."
    :around #'evil-org-open-below
    :around #'evil-org-open-above
    :around #'org-indent-region
    :around #'org-indent-line
    (save-window-excursion (apply fn args)))

  (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate
indentation."
    :after #'org-return
    (when (and indent
               org-src-tab-acts-natively
               (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  (defadvice! +org-inhibit-mode-hook-as (fn datum name &optional initialize &rest args)
    "Prevent potentially expensive mode hooks in
`org-babel-do-in-edit-buffer' ops."
    :around #'org-src--edit-element
    (apply fn datum name
           (if (and (eq org-src-window-setup 'switch-invisibly)
                    (functionp initialize))
               ;; org-babel-do-in-edit-buffer is used to execute quick, one-off
               ;; logic in the context of another major mode, but initializing a
               ;; major mode with expensive hooks can be terribly expensive.
               ;; Since Doom adds its most expensive hooks to
               ;; MAJOR-MODE-local-vars-hook, we can savely inhibit those.
               (lambda ()
                 (let ((zenit-inhibit-local-var-hooks t))
                   (funcall initialize)))
             initialize)
           args))

  ;; Refresh inline images after executing src blocks (useful for plantuml or
  ;; ipython, where the result could be an image)
  (defhook! +org-redisplay-inline-images-in-babel-result-h ()
    'org-babel-after-execute-hook
    (unless (or
             ;; ...but not while Emacs is exporting an org buffer (where
             ;; `org-display-inline-images' can be awfully slow).
             (bound-and-true-p org-export-current-backend)
             ;; ...and not while tangling org buffers (which happens in a temp
             ;; buffer where `buffer-file-name' is nil).
             (string-match-p "^ \\*temp" (buffer-name)))
      (save-excursion
        (when-let ((beg (org-babel-where-is-src-block-result))
                   (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
          (org-display-inline-images nil nil (min beg end) (max beg end))))))

  (after! python
    (unless (bound-and-true-p org-babel-python-command)
      (setq org-babel-python-command
            (string-trim
             (concat python-shell-interpreter " "
                     (if (string-match-p "\\<i?python[23]?$" python-shell-interpreter)
                         (replace-regexp-in-string
                          "\\(^\\| \\)-i\\( \\|$\\)" " " python-shell-interpreter-args)
                       python-shell-interpreter-args))))))

  (after! ob-ditaa
    ;; TODO Should be fixed upstream
    (let ((default-directory (org-find-library-dir "org-contribdir")))
      (setq org-ditaa-jar-path     (expand-file-name "scripts/ditaa.jar")
            org-ditaa-eps-jar-path (expand-file-name "scripts/DitaaEps.jar")))))


(defun +org-init-babel-lazy-loader-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun +org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
                 (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defadvice! +org--export-lazy-load-library-a (&optional element)
    "Lazy load a babel package when a block is executed during
 exporting."
    :before #'org-babel-exp-src-block
    (+org--babel-lazy-load-library-a (org-babel-get-src-block-info nil element)))

  (defadvice! +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    :before #'org-src--get-lang-mode
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))

  ;; This also works for tangling
  (defadvice! +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    :after-while #'org-babel-confirm-evaluate
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (+org--babel-lazy-load
       lang (and (not (assq :sync (nth 2 info)))
                 (assq :async (nth 2 info))))
      t))

  (advice-add #'org-babel-do-load-languages :override #'ignore))


(defun +org-init-capture-defaults-h ()
  "Sets up some reasonable defaults, as well as two
`org-capture' workflows that I like:

1. The traditional way: invoking `org-capture' directly, via SPC
   X, or through the :cap ex command.

2. Through a org-capture popup frame that is invoked from outside
   Emacs (the ~/.emacs.d/bin/org-capture script). This can be
   invoked from qutebrowser, vimperator, dmenu or a global
   keybinding."
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory)
        org-capture-templates
        `(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under,
          ;; e.g. :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  ;; Kill capture buffers by default (unless they've been visited)
  (after! org-capture
    (org-capture-put :kill-buffer t))

  ;; Fix #462: when refiling from org-capture, Emacs prompts to kill the
  ;; underlying, modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer)

  (defadvice! +org--remove-customize-option-a (orig-fn table title &optional prompt specials)
    "HACK Don't advertize `customize' as an option in
`org-capture's menu."
    :around #'org-mks
    (funcall orig-fn table title prompt
             (remove '("C" "Customize org-capture-templates")
                     specials)))

  (defadvice! +org--capture-expand-variable-file-a (file)
    "If a variable is used for a file path in
`org-capture-template', it is used as is, and expanded relative
to `default-directory'. This changes it to be relative to
`org-directory', unless it is an absolute path."
    :filter-args #'org-capture-expand-file
    (if (and (symbolp file) (boundp file))
        (expand-file-name (symbol-value file) org-directory)
      file))

  (defhook! +org-show-target-in-capture-header-h ()
    "Show target in capture header."
    'org-capture-mode-hook
    (setq header-line-format
          (format "%s%s%s"
                  (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                              'face 'font-lock-string-face)
                  org-eldoc-breadcrumb-separator
                  header-line-format)))

  (eval-when! (modulep! :editor evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state)))

(defun +org-init-capture-frame-h ()
  (add-hook 'org-capture-after-finalize-hook #'+org-capture-cleanup-frame-h)

  (defadvice! +org-capture-refile-cleanup-frame-a (&rest _)
    "Cleanup frame after `org-capture-refile'."
    :after #'org-capture-refile
    (+org-capture-cleanup-frame-h))

  (eval-when! (modulep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))


(defun +org-init-attachments-h ()
  "Sets up org's attachment system."
  (setq org-attach-store-link-p 'attached ; store link after attaching files
        org-attach-use-inheritance t)     ; inherit properties from parent nodes

  ;; Autoload all these commands that org-attach doesn't autoload itself
  (use-package! org-attach
    :commands (org-attach-new
               org-attach-open
               org-attach-open-in-emacs
               org-attach-reveal-in-emacs
               org-attach-url
               org-attach-set-directory
               org-attach-sync)
    :config
    (unless org-attach-id-dir
      ;; Centralized attachments directory by default
      (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
    (after! projectile
      (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

  ;; Add inline image previews for attachment links
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))


(defun +org-init-custom-links-h ()
  "Modify default file: links to colorize broken file links red"
  (org-link-set-parameters
   "file" :face (lambda (path)
                  (if (or (file-remote-p path)
                          ;; filter out network shares on windows (slow)
                          (eval-if! zenit--system-windows-p (string-prefix-p "\\\\" path))
                          (file-exists-p path))
                      'org-link
                    '(warning org-link))))

  ;; Add custom link types
  (pushnew! org-link-abbrev-alist
            '("github"      . "https://github.com/%s")
            '("youtube"     . "https://youtube.com/watch?v=%s")
            '("google"      . "https://google.com/search?q=")
            '("gimages"     . "https://google.com/images?q=%s")
            '("gmap"        . "https://maps.google.com/maps?q=%s")
            '("duckduckgo"  . "https://duckduckgo.com/?q=%s")
            '("wolfram"     . "https://wolframalpha.com/input/?i=%s")
            `("emacsdir"    . ,(zenit-path zenit-emacs-dir "%s"))
            `("confdir"     . ,(zenit-path zenit-local-conf-dir "%s")))

  (+org-define-basic-link "org" org-directory)
  (+org-define-basic-link "emacs" zenit-emacs-dir)
  (+org-define-basic-link "modules" zenit-modules-dir)

  (defadvice! +org-display-link-in-eldoc-a (&rest _)
    "Display full link in minibuffer when cursor/mouse is over it."
    :before-until #'org-eldoc-documentation-function
    (when-let (context (org-element-context))
      (if-let ((type (org-element-property :type context))
               (eldocfn (org-link-get-parameter type :eldoc)))
          (funcall eldocfn context)
        (when-let (raw-link (org-element-property :raw-link context))
          (format "Link: %s" raw-link)))))

  (defadvice! +org--follow-search-string-a (fn link &optional arg)
    "Support ::SEARCH syntax for id: links."
    :around #'org-id-open
    (save-match-data
      (cl-destructuring-bind (id &optional search)
          (split-string link "::")
        (prog1 (funcall fn id arg)
          (cond ((null search))
                ((string-match-p "\\`[0-9]+\\'" search)
                 ;; Move N lines after the ID (in case it's a heading), instead
                 ;; of the start of the buffer.
                 (forward-line (string-to-number option)))
                ((string-match "^/\\([^/]+\\)/$" search)
                 (let ((match (match-string 1 search)))
                   (save-excursion (org-link-search search))
                   ;; `org-link-search' only reveals matches. Moving the point
                   ;; to the first match after point is a sensible change.
                   (when (re-search-forward match)
                     (goto-char (match-beginning 0)))))
                ((org-link-search search)))))))

  ;; Allow inline image previews of http(s)? urls or data uris.
  ;; `+org-http-image-data-fn' will respect `org-display-remote-inline-images'.
  (setq org-display-remote-inline-images 'download) ; TRAMP urls
  (org-link-set-parameters "http"  :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "https" :image-data-fun #'+org-http-image-data-fn)
  (org-link-set-parameters "img"   :image-data-fun #'+org-inline-image-data-fn))


(defun +org-init-export-h ()
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t)

  (eval-when! (modulep! :lang markdown)
    (add-to-list 'org-export-backends 'md))

  (defadvice! +org--dont-trigger-save-hooks-on-export-a (orig-fn &rest args)
    "`org-export-to-file' triggers save hooks, which may
inadvertantly change the exported output (i.e. formatters)."
    :around '(org-export-to-file org-babel-tangle)
    (let (before-save-hook after-save-hook)
      (apply orig-fn args)))

  (defadvice! +org--fix-async-export-a (orig-fn &rest args)
    "TODO"
    :around '(org-export-to-file org-export-as)
    (let ((old-async-init-file org-export-async-init-file)
          (org-export-async-init-file (make-temp-file "emacs-org-async-export")))
      (with-temp-file org-export-async-init-file
        (prin1 `(progn (setq org-export-async-debug
                             ,(or org-export-async-debug
                                  debug-on-error)
                             load-path ',load-path)
                       (unwind-protect
                           (load ,(or old-async-init-file user-init-file)
                                 nil t)
                         (delete-file load-file-name)))
               (current-buffer)))
      (apply orig-fn args))))


(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Don't open separate windows
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in Emacs
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))

    ;; Open help:* links with helpful-* instead of describe-*
  (advice-add #'org-link--open-help :around #'zenit-use-helpful-a)

    ;; Unlike the stock showNlevels options, these will also show the parents of
  ;; the target level, recursively.
  (pushnew! org-startup-options
            '("show2levels*" org-startup-folded show2levels*)
            '("show3levels*" org-startup-folded show3levels*)
            '("show4levels*" org-startup-folded show4levels*)
            '("show5levels*" org-startup-folded show5levels*))

  (defadvice! +org--recursive-org-persist-mkdir-a (fn &rest args)
    "`org-persist-write:index' does not recursively create
`org-persist-directory', which causes an error if it's a parent
doesn't exist."
    :before #'org-persist-write:index
    (make-directory org-persist-directory t))

  (defadvice! +org--more-startup-folded-options-a ()
    "Adds support for 'showNlevels*' startup options.
Unlike showNlevels, this will also unfold parent trees."
    :before-until #'org-cycle-set-startup-visibility
    (when-let (n (pcase org-startup-folded
                   (`show2levels* 2)
                   (`show3levels* 3)
                   (`show4levels* 4)
                   (`show5levels* 5)))
      (org-fold-show-all '(headings))
      (save-excursion
        (goto-char (point-max))
        (save-restriction
          (narrow-to-region (point-min) (or (re-search-forward org-outline-regexp-bol nil t) (point-max)))
          (org-fold-hide-drawer-all))
        (goto-char (point-max))
        (let ((regexp (if (and (wholenump n) (> n 0))
                          (format "^\\*\\{%d,%d\\} " (1- n) n)
                        "^\\*+ "))
              (last (point)))
          (while (re-search-backward regexp nil t)
            (when (or (not (wholenump n))
                      (= (org-current-level) n))
              (org-fold-core-region (line-end-position) last t 'outline))
            (setq last (line-end-position 0)))))
      t))

  (defadvice! +org--respect-org-auto-align-tags-a (&rest _)
    "Some uses of `org-fix-tags-on-the-fly' occur without a check
on `org-auto-align-tags', such as in `org-self-insert-command'
and `org-delete-backward-char'."
    :before-while #'org-fix-tags-on-the-fly
    org-auto-align-tags)

  (defadvice! +org--recenter-after-follow-link-a (&rest _args)
    "Recenter after following a link, but only internal or file
links."
    :after '(org-footnote-action
             org-follow-timestamp-link
             org-link-open-as-file
             org-link-search)
    (when (get-buffer-window)
      (recenter)))

  (defadvice! +org--strip-properties-from-outline-a (orig-fn &rest args)
    "Fix variable height faces in eldoc breadcrumbs."
    :around #'org-format-outline-path
    (let ((org-level-faces
           (cl-loop for face in org-level-faces
                    collect `(:foreground ,(face-foreground face nil t)
                              :weight bold))))
      (apply orig-fn args)))

    (after! org-eldoc
    ;; HACK Fix #2972: infinite recursion when eldoc kicks in in 'org' or
    ;;      'python' src blocks.
    (puthash "org" #'ignore org-eldoc-local-functions-cache)
    (puthash "plantuml" #'ignore org-eldoc-local-functions-cache)
    (puthash "python" #'python-eldoc-function org-eldoc-local-functions-cache))

  (defun +org-restart-mode-h ()
    "Restart `org-mode', but only once."
    (quiet! (org-mode-restart))
    (delq! (current-buffer) org-agenda-new-buffers)
    (remove-hook 'zenit-switch-buffer-hook #'+org--restart-mode-h
                 'local)
    (run-hooks 'find-file-hook))

  (defhook! +org-exclude-agenda-buffers-from-workspace-h ()
    "Prevent temporary agenda buffers being associated with
current workspace."
    'org-agenda-finalize-hook
    (when (and org-agenda-new-buffers
               (bound-and-true-p bufferlo-mode)
               (not org-agenda-sticky))
      (dolist (buf org-agenda-new-buffers)
          (bufferlo-remove buf))))

  (defhook! +org-defer-mode-in-agenda-buffers-h ()
    "Org agenda opens temporary agenda incomplete org-mode buffers.
These are great for extracting agenda information from, but what
if the user tries to visit one of these buffers? Then we remove
it from the to-be-cleaned queue and restart `org-mode' so they
can grow up to be full-fledged org-mode buffers."
    'org-agenda-finalize-hook
    (dolist (buffer org-agenda-new-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (add-hook 'zenit-switch-buffer-hook #'+org--restart-mode-h
                    nil 'local)))))

  (defvar recentf-exclude)
  (defadvice! +org--exclude-agenda-buffers-from-recentf-a (orig-fn file)
    "Prevent temporarily opened agenda buffers from polluting
recentf."
    :around #'org-get-agenda-file-buffer
    (let ((recentf-exclude (list (lambda (_file) t)))
          (zenit-inhibit-large-file-detection t)
          org-startup-indented
          org-startup-folded
          vc-handled-backends
          org-mode-hook
          find-file-hook)
      (funcall orig-fn file)))

    ;; HACK With https://code.orgmode.org/bzg/org-mode/commit/48da60f4, inline
  ;;      image previews broke for users with imagemagick support built in. This
  ;;      reverses the problem, but should be removed once it is addressed
  ;;      upstream (if ever).
  (defadvice! +org--fix-inline-images-for-imagemagick-a (fn &rest args)
    :around #'org-display-inline-images
    (letf! (defun create-image (file-or-data &optional type data-p &rest props)
             (let ((type (if (plist-get props :width) type)))
               (apply create-image file-or-data type data-p props)))
      (apply fn args)))

  (defadvice! +org--fix-inconsistent-uuidgen-case-a (uuid)
    "Ensure uuidgen always produces lowercase output regardless of system."
    :filter-return #'org-id-new
    (if (eq org-id-method 'uuid)
        (downcase uuid)
      uuid)))


(defun +org-init-keybinds-h ()
  "Configure keybinds in `org-mode'."
  (add-hook 'zenit-escape-hook #'+org-remove-occur-highlights-h)

  ;; C-a & C-e act like `zenit/backward-to-bol-or-indent' and
  ;; `zenit/forward-to-last-non-comment-or-eol', but with more org awareness.
  (setq org-special-ctrl-a/e t)

  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)

  (add-hook 'zenit-delete-backward-functions
            #'+org-delete-backward-char-and-realign-table-maybe-h)

  (map!
   :map org-mode-map
        ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
        ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
        ;; that, and should probably be PRed to org.
        [tab]        #'org-cycle

   "C-c C-S-l"  #'+org/remove-link
   "C-c C-i"    #'org-toggle-inline-images
   ;; textmate-esque newline insertion
   "S-RET"      #'+org/shift-return
   "C-RET"      #'+org/insert-item-below
   "C-S-RET"    #'+org/insert-item-above
   "C-M-RET"    #'org-insert-subheading
   [C-return]   #'+org/insert-item-below
   [C-S-return] #'+org/insert-item-above
   [C-M-return] #'org-insert-subheading
   ;; Org-aware C-a/C-e
   [remap zenit/backward-to-bol-or-indent]          #'org-beginning-of-line
   [remap zenit/forward-to-last-non-comment-or-eol] #'org-end-of-line

   :localleader
        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        "@" #'org-cite-insert
   (:when (modulep! :completion vertico)
     "." #'consult-org-heading
     "/" #'consult-org-agenda)
   "A" #'org-archive-subtree
   "e" #'org-export-dispatch
   "f" #'org-footnote-action
   "h" #'org-toggle-heading
   "I" #'org-toggle-item
   "n" #'org-store-link
   "o" #'org-set-property
   "q" #'org-set-tags-command
   "t" #'org-todo
   "T" #'org-todo-list
   "X" #'org-toggle-checkbox
   (:prefix ("a" . "attachments")
         "a" #'org-attach
         "d" #'org-attach-delete-one
         "D" #'org-attach-delete-all
         "f" #'+org/find-file-in-attachments
         "l" #'+org/attach-file-and-insert-link
         "n" #'org-attach-new
         "o" #'org-attach-open
         "O" #'org-attach-open-in-emacs
         "r" #'org-attach-reveal
         "R" #'org-attach-reveal-in-emacs
         "u" #'org-attach-url
         "s" #'org-attach-set-directory
         "S" #'org-attach-sync
            (:when (modulep! :org org +dragndrop)
              "c" #'org-download-screenshot
              "p" #'org-download-clipboard
              "P" #'org-download-yank))
   (:prefix ("b" . "babel")
            (:when (modulep! :ui transient-state)
              "." #'zenit-org-babel-transient-state/body)
            "b" #'org-babel-execute-buffer
            "s" #'org-babel-execute-subtree
            "t" #'org-babel-tangle
            "f" #'org-babel-tangle-file)
   (:prefix ("B" . "tables")
            "-" #'org-table-insert-hline
            "a" #'org-table-align
            "b" #'org-table-blank-field
            "c" #'org-table-create-or-convert-from-region
            "e" #'org-table-edit-field
            "f" #'org-table-edit-formulas
            "h" #'org-table-field-info
            "s" #'org-table-sort-lines
            "r" #'org-table-recalculate
            "R" #'org-table-recalculate-buffer-tables
            (:prefix ("d" . "delete")
                     "c" #'org-table-delete-column
                     "r" #'org-table-kill-row)
            (:prefix ("i" . "insert")
                     "c" #'org-table-insert-column
                     "h" #'org-table-insert-hline
                     "r" #'org-table-insert-row
                     "H" #'org-table-hline-and-move)
            (:prefix ("t" . "toggle")
                     "f" #'org-table-toggle-formula-debugger
                     "o" #'org-table-toggle-coordinate-overlays))
   (:prefix ("d" . "date/deadline")
            "d" #'org-deadline
            "s" #'org-schedule
            "t" #'org-time-stamp
            "T" #'org-time-stamp-inactive)
   (:prefix ("g" . "goto")
    "g" #'org-goto
    (:when (modulep! :completion vertico)
      "g" #'consult-org-heading
      "G" #'consult-org-agenda)
    "c" #'org-clock-goto
    :desc "org-clock-goto 'select" "C" (cmd! (org-clock-goto 'select))
    "i" #'org-id-goto
    "r" #'org-refile-goto-last-stored
    "x" #'org-capture-goto-last-stored)
   (:prefix ("i" . "insert")
            "b" #'org-insert-structure-template
            "d" #'org-insert-drawer
            "f" #'org-footnote-action
            "h" #'org-insert-heading
            "H" #'org-insert-heading-after-current
            "i" #'org-insert-item
            "l" #'org-insert-link
            "n" #'org-add-note
            "s" #'org-insert-subheading)
   (:prefix ("l" . "links")
            "c" #'org-cliplink
            "d" #'+org/remove-link
            "i" #'org-id-store-link
            "l" #'org-insert-link
            "L" #'org-insert-all-links
            "s" #'org-store-link
            "S" #'org-insert-last-stored-link
            "t" #'org-toggle-link-display)
   (:prefix ("P" . "publish")
            "a" #'org-publish-all
            "f" #'org-publish-current-file
            "p" #'org-publish
            "P" #'org-publish-current-project
            "s" #'org-publish-sitemap)
   (:prefix ("r" . "refile")
            "." #'+org/refile-to-current-file
            "c" #'+org/refile-to-running-clock
            "l" #'+org/refile-to-last-location
            "f" #'+org/refile-to-file
            "o" #'+org/refile-to-other-window
            "O" #'+org/refile-to-other-buffer
            "v" #'+org/refile-to-visible
            "r" #'org-refile) ; to all `org-refile-targets'
   (:prefix ("s" . "tree/subtree")
            "a" #'org-toggle-archive-tag
            "b" #'org-tree-to-indirect-buffer
            "d" #'org-cut-subtree
            "h" #'org-promote-subtree
            "j" #'org-move-subtree-down
            "k" #'org-move-subtree-up
            "l" #'org-demote-subtree
            "n" #'org-narrow-to-subtree
            "r" #'org-refile
            "s" #'org-sparse-tree
            "A" #'org-archive-subtree
            "N" #'widen
            "S" #'org-sort)
   (:prefix ("p" . "priority")
            "d" #'org-priority-down
            "p" #'org-priority
            "u" #'org-priority-up)
   (:prefix ("x" . "text")
    :desc "bold" "b" (cmd! (org-emphasize ?\*))
    :desc "code"  "c" (cmd! (org-emphasize ?\~))
    :desc "italic" "i" (cmd! (org-emphasize ?\/))
    :desc "clear" "r" (cmd! (org-emphasize ?\ ))
    :desc "strike through" "s" (cmd! (org-emphasize ?\+))
    :desc "underline" "u" (cmd! (org-emphasize ?\_))
    :desc "verbatim" "v" (cmd! (org-emphasize ?\=)))))


(defun +org-init-popup-rules-h ()
  (set-popup-rules!
   '(("^\\*Org Links" :slot -1 :vslot -1 :size 2 :ttl 0)
     ("^ ?\\*\\(?:Agenda Com\\|Calendar\\|Org Export Dispatcher\\)"
      :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :ttl 0 :side right)
     ("^\\*Org \\(?:Select\\|Attach\\)" :slot -1 :vslot -2 :ttl 0 :size 0.25)
     ("^\\*Org Agenda" :ignore t)
     ;; ("^\\*Org Src"        :size 0.42  :quit nil :select t :autosave t :modeline t :ttl nil)
     ("^\\*Org Src" :ignore t)
     ("^\\*Org-Babel")
     ("^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.42 :quit nil :select t :autosave ignore))))


;;
;;; Packages

(use-package! toc-org
  :hook (org-mode . toc-org-enable)
  :config
  (setq toc-org-hrefify-default "gh")

  (defadvice! +org-inhibit-scrolling-a (orig-fn &rest args)
    "Prevent the jarring scrolling that occurs when the-ToC is
regenerated."
    :around #'toc-org-insert-toc
    (let ((p (set-marker (make-marker) (point)))
          (s (window-start)))
      (prog1 (apply orig-fn args)
        (goto-char p)
        (set-window-start nil s t)
        (set-marker p nil))))

  (eval-when! (modulep! :ui popup)
    (set-popup-rule! "\\*org-toc" :side 'right :width 0.3))
  (map!
   :localleader
   :map org-mode-map
   "TAB" #'org-toc-show))


(use-package evil-org
  :when (modulep! :editor evil)
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  ;; Only fold the current tree, rather than recursively
  (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h 'append)
  (map! :map evil-org-mode-map
        :ni [C-return]   #'+org/insert-item-below
        :ni [C-S-return] #'+org/insert-item-above
        ;; navigate table cells (from insert-mode)
        :i "C-l" (general-predicate-dispatch nil
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch nil
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch nil
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch nil
                   (org-at-table-p) 'org-table-next-row)
        ;; moving/(de|pro)moting subtress & expanding tables (prepend/append columns/rows)
        :ni "C-S-l" #'org-shiftright
        :ni "C-S-h" #'org-shiftleft
        :ni "C-S-k" #'org-shiftup
        :ni "C-S-j" #'org-shiftdown
        ;; more intuitive RET keybinds
        :i [return] (cmd! (org-return electric-indent-mode))
        :i "RET"    (cmd! (org-return electric-indent-mode))
        :i [S-return] #'+org/shift-return
        :i "S-RET"    #'+org/shift-return
        :n [return] #'+org/dwim-at-point
        :n "RET"    #'+org/dwim-at-point
        ;; more vim-esque org motion keys (not covered by evil-org-mode)
        :m "]h"  #'org-forward-heading-same-level
        :m "[h"  #'org-backward-heading-same-level
        :m "]l"  #'org-next-link
        :m "[l"  #'org-previous-link
        :m "]c"  #'org-babel-next-src-block
        :m "[c"  #'org-babel-previous-src-block
        :n "gQ"  #'org-fill-paragraph
        :n "gr"  #'org-ctrl-c-ctrl-c
        :n "gR"  #'org-babel-execute-buffer
        ;; sensible vim-esque folding keybinds
        :n "za"  #'+org/toggle-fold
        :n "zA"  #'org-shifttab
        :n "zc"  #'+org/close-fold
        :n "zC"  #'outline-hide-subtree
        :n "zm"  #'+org/hide-next-fold-level
        :n "zn"  #'org-tree-to-indirect-buffer
        :n "zo"  #'+org/open-fold
        :n "zO"  #'outline-show-subtree
        :n "zr"  #'+org/show-next-fold-level
        :n "zR"  #'outline-show-all
        :n "zi"  #'org-toggle-inline-images

        :map org-read-date-minibuffer-local-map
        "C-h"   (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
        "C-l"   (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
        "C-k"   (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
        "C-j"   (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
        "C-S-h" (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
        "C-S-l" (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
        "C-S-k" (cmd! (org-eval-in-calendar '(calendar-backward-year 1)))
        "C-S-j" (cmd! (org-eval-in-calendar '(calendar-forward-year 1)))))


;;
;;; Bootstrap

(use-package org
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :preface
  ;; Set these to nil now so we can detect user changes to them later (and
  ;; fall back on defaults otherwise)
  (defvar org-directory nil)
  (defvar org-id-locations-file nil)
  (defvar org-attach-id-dir nil)

  (setq org-publish-timestamp-directory (concat zenit-cache-dir "org-timestamps/")
        org-preview-latex-image-directory (concat zenit-cache-dir "org-latex/")
        org-persist-directory (concat zenit-cache-dir "org-persist/")
        ;; Recognize a), A), a., A., etc -- must be set before org is loaded.
        org-list-allow-alphabetical t)

  ;; Make most of the default modules opt-in to lighten its first-time load
  ;; delay.
  (defvar org-modules
    '(;; ol-w3m
      ;; ol-bbdb
      ol-bibtex
      ;; ol-docview
      ;; ol-gnus
      ;; ol-info
      ;; ol-irc
      ;; ol-mhe
      ;; ol-rmail
      ;; ol-eww
      ))

  (add-hook! 'org-mode-hook
             ;; `show-paren-mode' causes flickering with indent overlays made by
             ;; `org-indent-mode', so we turn off show-paren-mode altogether
             #'zenit-disable-show-paren-mode-h
             ;; disable `show-trailing-whitespace', shows a lot of false
             ;; positives
             #'zenit-disable-show-trailing-whitespace-h
             #'+org-enable-auto-reformat-tables-h
             #'+org-enable-auto-update-cookies-h
             #'+org-make-last-point-visible-h)

  (add-hook! 'org-load-hook
             #'+org-init-org-directory-h
             #'+org-init-appearance-h
             #'+org-init-attachments-h
             #'+org-init-babel-h
             #'+org-init-babel-lazy-loader-h
             #'+org-init-capture-defaults-h
             #'+org-init-capture-frame-h
             #'+org-init-custom-links-h
             #'+org-init-export-h
             #'+org-init-hacks-h
             #'+org-init-keybinds-h)

  (eval-when! (modulep! :ui popup)
    (add-hook! 'org-load-hook
               #'+org-init-popup-rules-h))

  ;; Wait until an org-protocol link is opened via emacsclient to load
  ;; `org-protocol'. Normally you'd simply require `org-protocol' and use it,
  ;; but the package loads all of org for no compelling reason, so...
  (defadvice! +org--server-visit-files-a (fn files &rest args)
    "Advise `server-visit-files' to load `org-protocol' lazily."
    :around #'server-visit-files
    (if (not (cl-loop with protocol =
                      (eval-if! zenit--system-windows-p
                          ;; On Windows, the file arguments for `emacsclient'
                          ;; get funnelled through `expand-file-path' by
                          ;; `server-process-filter'. This substitutes
                          ;; backslashes with forward slashes and converts each
                          ;; path to an absolute one. However, *all* absolute
                          ;; paths on Windows will match the regexp ":/+", so we
                          ;; need a more discerning regexp.
                          (regexp-quote
                           (or (bound-and-true-p org-protocol-the-protocol)
                               "org-protocol"))
                        ;; ...but since there is a miniscule possibility users
                        ;; have changed `org-protocol-the-protocol' I don't want
                        ;; this behavior for macOS/Linux users.
                        "")
                      for var in files
                      if (string-match-p (format "%s:/+" protocol) (car var))
                      return t))
        (apply fn files args)
      (require 'org-protocol)
      (apply #'org--protocol-detect-protocol-server fn files args)))
  (after! org-protocol
    (advice-remove 'server-visit-files #'org--protocol-detect-protocol-server))

  ;; In case the user has eagerly loaded org from their configs
  (when (and (featurep 'org)
             (not byte-compile-current-file))
    (unless (zenit-context-p 'reload)
      (message "`org' was already loaded by the time the module emacs/org loaded, this may cause issues"))
    (run-hooks 'org-load-hook))

  :config
  (add-to-list 'zenit-debug-variables 'org-export-async-debug)
  ;; Save target buffer after archiving
  (setq org-archive-subtree-save-file-p t)
  ;; Don't number headings with these tags
  (setq org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
        org-num-skip-tags '("noexport" "nonum"))
  ;; Prevent modifications made in invisible sections of an org document, as
  ;; unintended changes can easily go unseen otherwise.
  (setq org-catch-invisible-edits 'smart)
  ;; Global ID state means we can have ID links anywhere. This is required for
  ;; `org-brain', however.
  (setq org-id-locations-file-relative t)

  ;; HACK `org-id' doesn't check if `org-id-locations-file' exists or is
  ;;      writeable before trying to read/write to it.
  (defadvice! +org--fail-gracefully-a (&rest _)
    "Check if `org-id-locations-file' exists or is writeable
before trying to read/write to it."
    :before-while '(org-id-locations-save org-id-locations-load)
    (file-writable-p org-id-locations-file))

  (add-hook 'org-open-at-point-functions #'zenit-set-jump-h)
  ;; allow shift selection
  (setq org-support-shift-select t)
  ;; use latexmk for pdf export
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  ;; Inline images settings
  (setq org-image-actual-width 300)
  (add-hook! 'org-babel-after-execute-hook #'org-display-inline-images)
  (add-hook! 'org-mode-hook #'org-display-inline-images))
