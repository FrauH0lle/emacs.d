;; ui/vc-gutter/config.el -*- lexical-binding: t; -*-

;;
;;; Default styles

(eval-when! (modulep! +pretty)
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
                   16))
           (_ (if (zerop w) (setq w 16))))
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
  :hook (find-file    . diff-hl-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :commands diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk
  :config
  (set-popup-rule! "^\\*diff-hl" :select nil :size '+popup-shrink-to-fit)

  ;; A slightly faster algorithm for diffing.
  (setq vc-git-diff-switches '("--histogram"))
  ;; Slightly more conservative delay before updating the diff
  (setq diff-hl-flydiff-delay 0.5)  ; default: 0.3
  ;; Don't block Emacs when updating vc gutter
  (setq diff-hl-update-async t)

  ;; Get realtime feedback in diffs after staging/unstaging hunks.
  (setq diff-hl-show-staged-changes nil)

  ;; Update diffs when it makes sense too, without being too slow
  (eval-when! (modulep! :editor evil)
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
  (add-hook! '(zenit-escape-hook zenit-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `zenit-escape-hook' hooks."
      (ignore (or inhibit-redisplay
                  (and (or (bound-and-true-p diff-hl-mode)
                           (bound-and-true-p diff-hl-dir-mode))
                       (diff-hl-update-once))))))
  ;; Update diff-hl when magit alters git state.
  (eval-when! (modulep! :tools magit)
    (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
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

  ;; Don't delete the current hunk's indicators while we're editing
  (eval-when! (modulep! :editor evil)
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
