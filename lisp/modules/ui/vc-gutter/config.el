;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

;;
;;; Default styles

(static-when (modulep! +pretty)
  ;; Make the fringe small enough that the diff bars aren't too domineering,
  ;; while leaving enough room for other indicators.
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  ;; The gutter looks less cramped with some space between it and buffer.
  (setq-default fringes-outside-margins t)

  ;; Redefine fringe bitmaps to take up only half the horizontal space in the
  ;; fringe. This way we avoid overbearingly large diff bars without having to
  ;; shrink the fringe and sacrifice precious space for other fringe indicators
  ;; (like flycheck or flyspell).
  (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest _)
    :after #'diff-hl-define-bitmaps
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (h (+ (ceiling (* (frame-char-height) scale))
                 (if (floatp spacing)
                     (truncate (* (frame-char-height) spacing))
                   spacing)))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   diff-hl-bmp-max-width))
           (_ (if (zerop w) (setq w diff-hl-bmp-max-width))))
      (define-fringe-bitmap 'diff-hl-bmp-middle
        (make-vector
         h (string-to-number (let ((half-w (1- (/ w 2))))
                               (concat (make-string half-w ?1)
                                       (make-string (- w half-w) ?0)))
                             2))
        nil nil 'center)))

  (defun +vc-gutter-type-at-pos-fn (type _pos)
    (if (eq type 'delete)
        'diff-hl-bmp-delete
      'diff-hl-bmp-middle))
  (setq diff-hl-fringe-bmp-function #'+vc-gutter-type-at-pos-fn)
  (setq diff-hl-draw-borders nil)
  
  (add-hook! 'diff-hl-mode-hook
    (defun +vc-gutter-make-diff-hl-faces-transparent-h ()
      (mapc (zenit-rpartial #'set-face-background nil)
            '(diff-hl-insert
              diff-hl-delete
              diff-hl-change))))

  ;; To minimize overlap between flycheck indicators and diff-hl indicators in
  ;; the left fringe.
  (after! flycheck
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))


;;
;;; Packages

(use-package! diff-hl
  :hook (zenit-first-file . global-diff-hl-mode)
  :hook (vc-dir-mode . turn-on-diff-hl-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :init
  (add-hook! 'dired-mode-hook
    (defun +vc-gutter-enable-maybe-h ()
      "Conditionally enable `diff-hl-dired-mode' in dired buffers.
Respects `diff-hl-disable-on-remote'."
      ;; Neither `diff-hl-dired-mode' or `diff-hl-dired-mode-unless-remote'
      ;; respect `diff-hl-disable-on-remote', so...
      (unless (and (bound-and-true-p diff-hl-disable-on-remote)
                   (file-remote-p default-directory))
        (diff-hl-dired-mode +1))))

  :config
  (set-popup-rule! "^\\*diff-hl" :select nil)

  (setq diff-hl-global-modes '(not image-mode pdf-view-mode))
  ;; A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay (if (featurep :system 'macos) 1.0 0.5))  ; default: 0.3
  ;; Don't block Emacs when updating vc gutter
  (setq diff-hl-update-async (or (> emacs-major-version 30) 'thread))
  ;; Get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)


  ;; HACK: diff-hl exploits the auto-save mechanism to generate its temp file
  ;;   paths in /tmp (in `diff-hl-diff-buffer-with-reference'), which triggers
  ;;   an "autosave file in local temp dir, do you want to continue?" prompt
  ;;   anytime diff-hl wants to save one for TRAMP buffers.
  (defadvice! +vc-gutter--silence-temp-file-prompts-a (fn &rest args)
    :around #'diff-hl-diff-buffer-with-reference
    (let ((tramp-allow-unsafe-temporary-files t))
      (apply fn args)))

  ;; Update diffs when it makes sense too, without being too slow
  (static-when (modulep! :editor evil)
    (map! :after diff-hl-show-hunk
          :map diff-hl-show-hunk-map
          :n "p" #'diff-hl-show-hunk-previous
          :n "n" #'diff-hl-show-hunk-next
          :n "c" #'diff-hl-show-hunk-copy-original-text
          :n "r" #'diff-hl-show-hunk-revert-hunk
          :n "[" #'diff-hl-show-hunk-previous
          :n "]" #'diff-hl-show-hunk-next
          :n "{" #'diff-hl-show-hunk-previous
          :n "}" #'diff-hl-show-hunk-next
          :n "S" #'diff-hl-show-hunk-stage-hunk))
  ;; Refresh gutter on ESC or refocusing the Emacs frame.
  (defvar-local +vc-gutter--last-state nil)
  (add-hook! '(zenit-escape-hook zenit-switch-window-hook zenit-switch-frame-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `zenit-escape-hook' hooks."
      (when-let* (((or (bound-and-true-p diff-hl-mode)
                       (bound-and-true-p diff-hl-dir-mode)))
                  (file (buffer-file-name (buffer-base-buffer)))
                  ((not ; debouncing
                    (equal (cons (point) +vc-gutter--last-state)
                           (setq +vc-gutter--last-state
                                 (cons (point)
                                       (copy-sequence
                                        (symbol-plist
                                         (intern (expand-file-name file)
                                                 vc-file-prop-obarray)))))))))
        (ignore (diff-hl-update)))))
  ;; Update diff-hl when magit alters git state.
  (static-when (modulep! :tools magit)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; The revert popup consumes 50% of the frame, whether or not you're reverting
  ;; 2 lines or 20. This resizes the popup to match its contents.
  (defadvice! +vc-gutter--shrink-popup-a (fn &rest args)
    :around #'diff-hl-revert-hunk-1
    (letf! ((refine-mode diff-auto-refine-mode)
            (diff-auto-refine-mode t)
            (defun diff-refine-hunk ()
              (when refine-mode
                (funcall diff-refine-hunk))
              (shrink-window-if-larger-than-buffer)))
      (apply fn args)))

  ;; Update diff-hl immediately upon exiting insert mode.
  (static-when (modulep! :editor evil)
    (add-hook! 'diff-hl-flydiff-mode-hook
      (defun +vc-gutter-init-flydiff-mode-h ()
        (if diff-hl-flydiff-mode
            (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
          (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)))))

  ;; Reverting a hunk causes the cursor to be moved to an unexpected place,
  ;; often far from the target hunk.
  (defadvice! +vc-gutter--save-excursion-a (fn &rest args)
    "Suppresses unexpected cursor movement by `diff-hl-revert-hunk'."
    :around #'diff-hl-revert-hunk
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt)))))
