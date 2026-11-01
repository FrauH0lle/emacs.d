;; editor/evil/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `zenit/escape' if `evil-force-normal-state' is called
interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'zenit/escape)))

;;;###autoload
(defun +evil--mouse-drag-region-a (fn start-event)
  "Keep single clicks handled by FN out of Visual state.
Ignore focus events and motion below `double-click-fuzz' while tracking
START-EVENT. Compare mouse coordinates, not text positions: redisplay can
move text underneath a stationary pointer. Preserve real drags and
multiple-click selections."
  (let* ((start (event-start start-event))
         (window (posn-window start))
         (xy (posn-x-y start)))
    (if (or (/= (event-click-count start-event) 1)
            (not (window-live-p window))
            (not (and (numberp (car-safe xy)) (numberp (cdr-safe xy)))))
        (funcall fn start-event)
      (let ((read-key-fn (symbol-function 'read-key))
            ;; Terminal coordinates use characters, but fuzz uses eighths.
            (units (if (display-graphic-p (window-frame window)) 1 8))
            dragged)
        (cl-letf (((symbol-function 'read-key)
                   (lambda (&rest args)
                     (let (event)
                       (while
                           (progn
                             (setq event (apply read-key-fn args))
                             (when (and (not dragged) (mouse-event-p event))
                               (let* ((end (event-end event))
                                      (end-xy (posn-x-y end)))
                                 (setq dragged
                                       (or (not (eq window (posn-window end)))
                                           (not (and (numberp (car-safe end-xy))
                                                     (numberp (cdr-safe end-xy))))
                                           (>= (* units (abs (- (car end-xy) (car xy))))
                                               double-click-fuzz)
                                           (>= (* units (abs (- (cdr end-xy) (cdr xy))))
                                               double-click-fuzz)))))
                             ;; Evil otherwise enters Visual even for these
                             ;; events, then treats redisplay as a text drag.
                             (or (memq (car-safe event) '(switch-frame select-window))
                                 (and (mouse-movement-p event) (not dragged))))
                         nil)
                       ;; Native input can label a stationary release a drag
                       ;; solely because redisplay changed its text position.
                       (if (and (not dragged)
                                (memq 'drag (event-modifiers event)))
                           (list (event-convert-list
                                  (append (remq 'drag (event-modifiers event))
                                          (list (event-basic-type event))))
                                 (event-end event))
                         event)))))
          (prog1 (funcall fn start-event)
            (when (and (not dragged) (eq window (selected-window)))
              (when (and (evil-visual-state-p) (eq evil-visual-selection 'char))
                (evil-exit-visual-state))
              ;; set-mark queues Evil's delayed Visual activation. Clear the
              ;; mark even when the tracker returned in Normal state.
              (deactivate-mark t))))))))

;;;###autoload
(defun +evil-replace-filename-modifiers-a (file-name)
  "Take a path and resolve any vim-like filename modifiers in it.
This adds support for most vim file modifiers, as well as:

  %:P   Resolves to `zenit-project-root'.

See
http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers
for more information on modifiers."
  (let ((origin-buffer (current-buffer))
        case-fold-search)
    (with-temp-buffer
      (let ((buffer-file-name (buffer-file-name origin-buffer)))
        (save-excursion (insert file-name))
        (while (re-search-forward "\\(^\\|[^\\\\]\\)\\(\\([%#]\\)\\(:\\([PphtreS~.]\\|g?s\\)\\)*\\)" nil t)
          (if (null buffer-file-name)
              (replace-match (match-string 1) t t nil 2)
            (let ((beg (match-beginning 2))
                  (end (match-end 3))
                  (path (pcase (match-string 3)
                          ("%" (file-relative-name buffer-file-name default-directory))
                          ("#" (and (other-buffer origin-buffer)
                                    (buffer-file-name (other-buffer origin-buffer)))))))
              (save-match-data
                (goto-char beg)
                (while (re-search-forward ":\\([PphtreS~.]\\|g?s\\)" (+ (point) 3) t)
                  (let* ((modifier (match-string 1))
                         (global (string-prefix-p "gs" modifier)))
                    (when global
                      (setq modifier (substring modifier 1)))
                    (setq end (match-end 1)
                          path
                          (pcase (and path (substring modifier 0 1))
                            (`nil "")
                            ("p" (expand-file-name path))
                            ("~" (concat "~/" (file-relative-name path "~")))
                            ("." (file-relative-name path))
                            ("t" (file-name-nondirectory (directory-file-name path)))
                            ("r" (file-name-sans-extension path))
                            ("e" (file-name-extension path))
                            ("S" (shell-quote-argument path))
                            ("h"
                             (let ((parent (file-name-directory (expand-file-name path))))
                               (unless (file-equal-p path parent)
                                 (if (file-name-absolute-p path)
                                     (directory-file-name parent)
                                   (file-relative-name parent)))))
                            ("s"
                             (if (featurep 'evil)
                                 (when-let* ((args (evil-delimited-arguments (substring modifier 1) 2)))
                                   (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                         (replace (cadr args)))
                                     (replace-regexp-in-string
                                      (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                      (evil-transform-vim-style-regexp replace) path t t
                                      (unless global 1))))
                               path))
                            ("P"
                             (let ((project-root (zenit-project-root (file-name-directory (expand-file-name path)))))
                               (unless project-root
                                 (user-error "Not in a project"))
                               (abbreviate-file-name project-root)))))
                    ;; strip trailing slash, if applicable
                    (or (string-empty-p path)
                        (not (equal (substring path -1) "/"))
                        (setq path (substring path 0 -1))))))
              (replace-match path t t nil 2))))
        (replace-regexp-in-string "\\\\\\([#%]\\)" "\\1" (buffer-string) t)))))

(defun +evil--insert-newline (&optional above _noextranewline)
  (let ((pos (save-excursion (beginning-of-line-text) (point)))
        comment-auto-fill-only-comments)
    (evil-narrow-to-field
      (if above
          (if (save-excursion (nth 4 (zenit-syntax-ppss pos)))
              (evil-save-goal-column
                (setq evil-auto-indent nil)
                (goto-char pos)
                (let ((ws (abs (skip-chars-backward " \t"))))
                  ;; FIXME oh god why
                  (save-excursion
                    (if comment-line-break-function
                        (funcall comment-line-break-function nil)
                      (comment-indent-new-line))
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode 'js2-mode)
                               (eq (char-after) ?/))
                      (insert "*"))
                    (insert
                     (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                  32)))
                  (insert (make-string (max 1 ws) 32))))
            (evil-move-beginning-of-line)
            (insert (if use-hard-newlines hard-newline "\n"))
            (forward-line -1)
            (back-to-indentation))
        (evil-move-end-of-line)
        (cond ((zenit-point-in-comment-p pos)
               (setq evil-auto-indent nil)
               (if comment-line-break-function
                   (funcall comment-line-break-function nil)
                 (comment-indent-new-line)))
              ;; TODO Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

;;;###autoload
(defun +evil--insert-newline-below-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun! evil-insert-newline-below () (+evil--insert-newline))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload
(defun +evil--insert-newline-above-and-respect-comments-a (fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p)
          (evil-emacs-state-p))
      (funcall fn count)
    (letf! (defun! evil-insert-newline-above () (+evil--insert-newline 'above))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall fn count)))))

;;;###autoload (autoload '+evil-window-split-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-split-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-window-vsplit-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-vsplit-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((origwin (selected-window))
        window-selection-change-functions)
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin)))
  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-join-a "editor/evil/autoload/advice" nil nil)
(defun +evil-join-a (fn beg end)
  "Join the selected lines.

This advice improves on `evil-join' by removing comment
delimiters when joining commented lines, without
`fill-region-as-paragraph'.

Adapted from URL `https://github.com/emacs-evil/evil/issues/606'."
  (if-let* (((not (= (line-end-position) (point-max))))
            (cend (save-excursion (goto-char end) (line-end-position)))
            (cbeg (save-excursion
                    (goto-char beg)
                    (and (zenit-point-in-comment-p
                          (save-excursion
                            (goto-char (line-beginning-position 2))
                            (skip-syntax-forward " \t")
                            (point)))
                         (or (comment-search-backward (line-beginning-position) t)
                             (comment-search-forward  (line-end-position) t)
                             (and (zenit-point-in-comment-p beg)
                                  (stringp comment-continue)
                                  (or (search-forward comment-continue (line-end-position) t)
                                      beg)))))))
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (uncomment-region (line-beginning-position 2)
                          (save-excursion
                            (goto-char cend)
                            (line-end-position 0)))
        (unwind-protect
            (dotimes (_ count)
              (join-line 1)
              (save-match-data
                (when (or (and comment-continue
                               (not (string-empty-p comment-continue))
                               (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                          (and comment-start-skip
                               (not (string-empty-p comment-start-skip))
                               (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                  (replace-match "" t nil nil 1)
                  (just-one-space))))
          (set-marker fixup-mark nil)))
    ;; But revert to the default we're not in a comment, where
    ;; `fill-region-as-paragraph' is too greedy.
    (funcall fn beg end)))

;;;###autoload
(defun +evil--fix-dabbrev-in-minibuffer-h ()
  "Make `try-expand-dabbrev' from `hippie-expand' work in
minibuffer. See `he-dabbrev-beg', so we need to redefine syntax
for '/'."
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))
