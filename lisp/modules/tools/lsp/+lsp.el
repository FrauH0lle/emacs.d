;; tools/lsp/+lsp.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! lsp-mode
  :commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (file-name-concat zenit-cache-dir "lsp-session")
        lsp-server-install-dir (file-name-concat zenit-data-dir "lsp"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;   features opt-in. Some servers implement these poorly and, in most cases,
  ;;   it's safer to rely on Emacs' native mechanisms (eldoc vs lsp-ui-doc, open
  ;;   in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Enable inlay hints
  (setq lsp-inlay-hint-enable t)

  ;; Explicitly tell lsp to use flymake; Lsp will default to flycheck if found
  ;; even if its a dependency
  (when (modulep! :checkers syntax +flymake)
    (setq lsp-diagnostics-provider :flymake))

  ;; Let us bind the lsp keymap.
  (static-when (modulep! :config default +bindings)
    (setq lsp-keymap-prefix nil))

  :config
  (set-debug-variable! 'lsp-log-io t 2)

  (setq lsp-intelephense-storage-path (concat zenit-data-dir "lsp-intelephense/")
        lsp-vetur-global-snippets-dir
        (expand-file-name
         "vetur" (or (bound-and-true-p +snippets-dir)
                     (concat zenit-local-conf-dir "snippets/")))
        lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
        lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

  ;; REVIEW Remove this once this is fixed upstream.
  (add-to-list 'lsp-client-packages 'lsp-racket)

  (add-hook! 'zenit-escape-hook
    (defun +lsp-signature-stop-maybe-h ()
      "Close the displayed `lsp-signature'."
      (when lsp-signature-mode
        (lsp-signature-stop)
        t)))

  (set-popup-rules!
    '(("^\\*lsp-install" :size 0.35 :quit t :select t)
      ("^\\*lsp-help" :size 0.35 :quit t :select t :tabbed t)))
  (static-when (modulep! :tools lookup)
    (set-lookup-handlers! 'lsp-mode
                          :definition #'+lsp-lookup-definition-handler
                          :references #'+lsp-lookup-references-handler
                          :documentation '(lsp-describe-thing-at-point :async t)
                          :implementations '(lsp-find-implementation :async t)
                          :type-definition #'lsp-find-type-definition))

  (defadvice! +lsp--respect-user-defined-checkers-a (fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by
`lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply fn args)
          (setq-local flycheck-checker old-checker))
      (apply fn args)))

  (add-hook 'lsp-before-initialize-hook #'+lsp-optimization-mode)
  (add-hook! 'lsp-after-uninitialized-functions
    (defun +lsp--disable-optimization-mode-if-no-workspaces-h (_workspace)
      (unless (lsp--session-workspaces lsp--session)
        (+lsp-optimization-mode -1))))

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before
the server is auto-killed (which is a potentially expensive
process). It also prevents the server getting expensively
restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (funcall fn restart)
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspaces)
                   (dolist (ws workspaces)
                     (or (cl-some #'lsp-buffer-live-p
                                  (lsp--workspace-buffers ws))
                         (with-lsp-workspace ws
                                             (let ((lsp-restart 'ignore))
                                               (funcall fn))))))
             lsp--buffer-workspaces))))

  (static-when (modulep! :ui modeline +light)
    (defvar-local lsp-modeline-icon nil)

    (add-hook! '(lsp-before-initialize-hook
                 lsp-after-initialize-hook
                 lsp-after-uninitialized-functions
                 lsp-before-open-hook
                 lsp-after-open-hook)
      (defun +lsp-update-modeline (&rest _)
        "Update modeline with lsp state."
        (let* ((workspaces (lsp-workspaces))
               (face (if workspaces 'success 'warning))
               (label (if workspaces "LSP Connected" "LSP Disconnected")))
          (setq lsp-modeline-icon (concat
                                   " "
                                   (+modeline-format-icon 'faicon "rocket" "" face label -0.0575)
                                   " "))
          (add-to-list 'global-mode-string
                       '(t (:eval lsp-modeline-icon))
                       'append)))))

  (static-when (modulep! :completion corfu)
    (setq lsp-completion-provider :none)
    (add-hook 'lsp-mode-hook #'lsp-completion-mode)))


(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (defadvice! +lsp--use-hook-instead-a (fn &rest args)
    "Change `lsp--auto-configure' to not force `lsp-ui-mode' on us. Using a hook
instead is more sensible."
    :around #'lsp--auto-configure
    (letf! ((#'lsp-ui-mode #'ignore))
      (apply fn args)))

  :config
  (static-when (modulep! +peek)
    (static-when (modulep! :tools lookup)
      (set-lookup-handlers! 'lsp-ui-mode
                            :definition 'lsp-ui-peek-find-definitions
                            :implementations 'lsp-ui-peek-find-implementation
                            :references 'lsp-ui-peek-find-references
                            :async t)))

  (setq lsp-ui-peek-enable (modulep! +peek)
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72         ; 150 (default) is too wide
        lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
        lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        ;; Don't show symbol definitions in the sideline. They are pretty noisy,
        ;; and there is a bug preventing Flycheck errors from being shown (the
        ;; errors flash briefly and then disappear).
        lsp-ui-sideline-show-hover nil
        ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
        ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  (map! :map lsp-ui-peek-mode-map
        "j"   #'lsp-ui-peek--select-next
        "k"   #'lsp-ui-peek--select-prev
        "C-k" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next-file))


(use-package! consult-lsp
  :defer t
  :when (modulep! :completion vertico)
  :init
  (map! :map lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))
