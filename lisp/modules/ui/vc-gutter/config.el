;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

;; TODO Implement me
(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open
  through TRAMP).")


;;
;;; Default styles

;; Make the fringe small enough that the diff bars aren't too domineering,
;; while leaving enough room for other indicators.
(if (fboundp 'fringe-mode) (fringe-mode '8))
;; The gutter looks less cramped with some space between it and buffer.
(setq-default fringes-outside-margins t)

;; Redefine fringe bitmaps to take up only half the horizontal space in the
;; fringe. This way we avoid overbearingly large diff bars without having to
;; shrink the fringe and sacrifice precious space for other fringe indicators
;; (like flycheck or flyspell).
(after! git-gutter-fringe
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

;; To minimize overlap between flycheck indicators and git-gutter/diff-hl
;; indicators in the left fringe.
(after! flycheck
  ;; Let diff-hl have left fringe, flycheck can have right fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))


;;
;;; git-gutter

(use-package! git-gutter
  :commands
  git-gutter:revert-hunk git-gutter:stage-hunk
  git-gutter:next-hunk git-gutter:previous-hunk
  :init
  (defhook! +vc-gutter-init-maybe-h ()
    "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file,
`git-gutter-mode's activation is deferred until the file is
saved. Respects `git-gutter:disabled-modes'."
    'find-file-hook
    (let ((file-name (buffer-file-name (buffer-base-buffer))))
      (cond
       ((and (file-remote-p (or file-name default-directory))
             (not +vc-gutter-in-remote-files)))
       ;; UX: If not a valid file, wait until it is written/saved to activate
       ;;   git-gutter.
       ((not (and file-name (vc-backend file-name)))
        (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
       ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
       ;;   type of frame we're in. This allows git-gutter to work for silly
       ;;   geese who open both tty and gui frames from the daemon.
       ((if (and (display-graphic-p)
                 (require 'git-gutter-fringe nil t))
            (setq-local git-gutter:init-function      #'git-gutter-fr:init
                        git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                        git-gutter:clear-function     #'git-gutter-fr:clear
                        git-gutter:window-width -1)
          (setq-local git-gutter:init-function      'nil
                      git-gutter:view-diff-function #'git-gutter:view-diff-infos
                      git-gutter:clear-function     #'git-gutter:clear-diff-infos
                      git-gutter:window-width 1))
        (unless (memq major-mode git-gutter:disabled-modes)
          (git-gutter-mode +1)
          (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local))))))

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  (eval-when! (modulep! :ui popup)
    (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit))

  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-function :after after-focus-change-function #'git-gutter:update-all-windows)

  (defhook! +vc-gutter-update-h (&rest _)
    "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`zenit-escape-hook' hooks."
    '(zenit-escape-hook zenit-switch-window-hook) :append
    (ignore (or (memq this-command '(git-gutter:stage-hunk
                                     git-gutter:revert-hunk))
                inhibit-redisplay
                (if git-gutter-mode
                    (git-gutter)
                  (+vc-gutter-init-maybe-h)))))
  ;; UX: update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  ;; FIX: stop git-gutter:{next,previous}-hunk from jumping to random hunks.
  (defadvice! +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
    :override #'git-gutter:search-near-diff-index
    (cl-position-if (let ((lineno (line-number-at-pos))
                          (fn (if is-reverse #'> #'<)))
                      (lambda (line) (funcall fn lineno line)))
                    diffinfos
                    :key #'git-gutter-hunk-start-line
                    :from-end is-reverse)))
