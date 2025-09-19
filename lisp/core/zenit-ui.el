;; lisp/core/zenit-keybinds.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `ansi-color'
(defvar ansi-color-for-comint-mode)

;; `cl-seq'
(declare-function cl-delete-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-set-difference "cl-seq" (cl-list1 cl-list2 &rest cl-keys))
(declare-function cl-union "cl-seq" (cl-list1 cl-list2 &rest cl-keys))

;; `hide-mode-line'
(declare-function hide-mode-line-mode "ext:hide-mode-line" (&optional arg))

;; `image'
(defvar image-animate-loop)

;; `profiler'
(defvar profiler-report-cpu-line-format)
(defvar profiler-report-memory-line-format)

;; `rainbow-delimiters'
(defvar rainbow-delimiters-max-face-count)

;; `whitespace'
(defvar whitespace-active-style)
(defvar whitespace-display-mappings)
(defvar whitespace-line-column)
(defvar whitespace-mode)
(defvar whitespace-style)

;; `zenit-core'
(declare-function zenit--reset-inhibited-vars-h "zenit-core" ())

;; `zenit-ui'
(declare-function zenit-run-switch-frame-hooks-fn "zenit-ui" ())

;; `zenit-lib-buffers'
(declare-function zenit-fallback-buffer "zenit-lib-buffers" ())
(declare-function zenit-visible-buffers "zenit-lib-buffers" (&optional buffer-list all-frames))
(declare-function zenit-real-buffer-list "zenit-lib-buffers" (&optional buffer-list))
(declare-function zenit-real-buffer-p "zenit-lib-buffers" (buffer-or-name))

;; `zenit-lib-buffers'
(declare-function zenit/delete-frame-with-prompt "zenit-lib-ui" ())
(declare-function zenit-quit-p "zenit-lib-ui" (&optional prompt))


;;
;;; Variables

(defcustom zenit-theme nil
  "The Emacs theme or themes to load at startup.
Is either a symbol representing the name of an Emacs theme, or a
list thereof (to enable in order).

Set to nil to load no theme at all. This variable is changed by
`load-theme' and `enable-theme'."
  :group 'zenit
  :type 'symbol)

(defcustom zenit-font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an
XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq zenit-font (font-spec :family \"Fira Mono\" :size 12))
  (setq zenit-font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq zenit-font \"Fira Code-14\")"
  :group 'zenit
  :type 'string)

(defcustom zenit-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an
XLFD string. See `zenit-font' for examples.

An omitted font size means to inherit `zenit-font''s size."
  :group 'zenit
  :type 'string)

(defcustom zenit-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an
XLFD string. See `zenit-font' for examples.

An omitted font size means to inherit `zenit-font''s size."
  :group 'zenit
  :type 'string)

(defcustom zenit-symbol-font nil
  "Fallback font for symbols.
Must be a `font-spec', a font object, an XFT font string, or an
XLFD string. See `zenit-font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and
Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock
any usage of this font to that size. It's rarely a good idea to
do so!"
  :group 'zenit
  :type 'string)

(defcustom zenit-emoji-font nil
  "Fallback font for emoji.
Must be a `font-spec', a font object, an XFT font string, or an
XLFD string. See `zenit-font' for examples.

WARNING: if you specify a size for this font it will hard-lock
any usage of this font to that size. It's rarely a good idea to
do so!"
  :group 'zenit
  :type 'string)

(defconst zenit-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.
These are platform-specific fallbacks for internal use. If you
want to change your emoji font, use `zenit-emoji-font'.")

(defconst zenit-symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.
These are platform-specific fallbacks for internal use. If you
want to change your symbol font, use `zenit-symbol-font'.")


;;
;;; Custom hooks

(defcustom zenit-init-ui-hook nil
  "List of hooks to run when the UI has been initialized."
  :group 'zenit
  :type 'hook)

(defcustom zenit-load-theme-hook nil
  "Hook run after the theme is loaded.
Either via `load-theme' or reloaded with `zenit/reload-theme'."
  :group 'zenit
  :type 'hook)

(defcustom zenit-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer."
  :group 'zenit
  :type 'hook)

(defcustom zenit-switch-window-hook nil
  "A list of hooks run after changing the focused windows."
  :group 'zenit
  :type 'hook)

(defcustom zenit-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.

This also serves as an analog for `after-focus-change-function',
but also preforms debouncing (see
`zenit-switch-frame-hook-debounce-delay'). It's possible for this
hook to be triggered multiple times (because there are edge cases
where Emacs can have multiple frames focused at once)."
  :group 'zenit
  :type 'hook)

(defun zenit-run-switch-buffer-hooks-h (&optional _)
  "Run hooks associated with buffer switching.

This function runs all hooks registered with
`zenit-switch-buffer-hook'.

The function takes an optional argument, which is ignored. This
allows it to be used in contexts where a function with one
argument is expected, such as in `add-hook'."
  (let ((gc-cons-threshold most-positive-fixnum))
    (run-hooks 'zenit-switch-buffer-hook)))

(defun zenit-run-switch-window-hooks-h (&optional _)
  "Run hooks associated with window switching.

Trigger `zenit-switch-window-hook' when selecting a window in the
same frame."
  (unless (or (minibufferp)
              (not (equal (old-selected-frame) (selected-frame)))
              (equal (old-selected-window) (selected-window))
              (equal (old-selected-window) (minibuffer-window)))
    (let ((gc-cons-threshold most-positive-fixnum))
      (run-hooks 'zenit-switch-window-hook))))

(defvar zenit-switch-frame-hook-debounce-delay 2.0
  "The delay for which `zenit-switch-frame-hook' won't trigger again.

This exists to prevent switch-frame hooks getting triggered too aggressively due
to misbehaving desktop environments, packages incorrectly frame switching in
non-interactive code, or the user accidentally (and rapidly) un-and-refocusing
the frame through some other means.")

(defun zenit--run-switch-frame-hooks-fn (_)
  "Run `zenit-switch-frame-hook'."
  (remove-hook 'pre-redisplay-functions #'zenit--run-switch-frame-hooks-fn)
  (let ((gc-cons-threshold most-positive-fixnum))
    (dolist (fr (visible-frame-list))
      (let ((state (frame-focus-state fr)))
        (when (and state (not (eq state 'unknown)))
          (let ((last-update (frame-parameter fr '+last-focus)))
            (when (or (null last-update)
                      (> (float-time (time-subtract (current-time) last-update))
                         zenit-switch-frame-hook-debounce-delay))
              (with-selected-frame fr
                (unwind-protect
                    (run-hooks 'zenit-switch-frame-hook)
                  (set-frame-parameter fr '+last-focus (current-time)))))))))))

(let (last-focus-state)
  (defun zenit-run-switch-frame-hooks-fn ()
    "Trigger `zenit-switch-frame-hook' once per frame focus change."
    (or (equal last-focus-state
               (setq last-focus-state
                     (mapcar #'frame-focus-state (frame-list))))
        ;; Defer until next redisplay
        (add-hook 'pre-redisplay-functions #'zenit--run-switch-frame-hooks-fn))))

(defun zenit-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer.

Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (zenit-fallback-buffer))))

(defun zenit-highlight-non-default-indentation-h ()
  "Highlight whitespace at odds with `indent-tabs-mode'.

That is, highlight tabs if `indent-tabs-mode' is nil, and highlight
spaces at the beginnings of lines if `indent-tabs-mode' is t. The
purpose is to make incorrect indentation in the current buffer obvious
to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already
active or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (cl-union (if indent-tabs-mode
                       '(indentation)
                     '(tabs tab-mark))
                   (when whitespace-mode
                     (remq 'face whitespace-active-style))))
    (cl-pushnew 'face whitespace-style) ; must be first
    (whitespace-mode +1)))


;;
;;; General UX

;; A simple confirmation prompt when killing Emacs. But only prompt when there
;; are real buffers open.
(setq confirm-kill-emacs #'zenit-quit-p)

;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'zenit/delete-frame-with-prompt)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Larger column width for function name in profiler reports
(with-eval-after-load 'profiler
  ;; (after! profiler
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Buffers

(defadvice! zenit--switch-to-fallback-buffer-maybe-a (&rest _)
  "Switch to `zenit-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window,
delete it. If there are no real buffers left OR if all remaining
buffers are visible in other windows, switch to
`zenit-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  :before-until #'kill-current-buffer
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window)
           t)
          ((eq buf (zenit-fallback-buffer))
           (message "Can't kill the fallback buffer.")
           t)
          ((and (zenit-real-buffer-p buf)
                (run-hook-with-args-until-failure 'kill-buffer-query-functions))
           (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
             (unless visible-p
               (when (and (buffer-modified-p buf)
                          (not (y-or-n-p
                                (format "Buffer %s is modified; kill anyway?"
                                        buf))))
                 (user-error "Aborted")))
             (let ((inhibit-redisplay t)
                   buffer-list-update-hook
                   kill-buffer-query-functions)
               (when (or
                      ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (zenit-real-buffer-list)
                                              (zenit-visible-buffers nil t)))
                      ;; If we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go.
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (zenit-fallback-buffer)))
               (unless visible-p
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil))
                 (kill-buffer buf)))
             (run-hooks 'buffer-list-update-hook)
             t)))))


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; GUIs are inconsistent across systems, desktop environments, and themes, and
;; don't match the look of Emacs. They also impose inconsistent shortcut key
;; paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'zenit-init-ui-hook #'window-divider-mode)

;; Favor vertical splits over horizontal ones.
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      tooltip-resize-echo-area t)

;; Typing yes/no is obnoxious when y/n will do
(if (boundp 'use-short-answers)
    (setq use-short-answers t))
;; HACK: By default, SPC = yes when `y-or-n-p' prompts you (and
;;   `y-or-n-p-use-read-key' is off). This seems too easy to hit by accident,
;;   especially with SPC as our default leader key.
(define-key y-or-n-p-map " " nil)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(after! comint
  ;; Double the default
  (setq-default comint-buffer-maximum-size 2048)

  ;; Temporarily disable undo history between command executions. Otherwise,
  ;; undo could destroy output while it's being printed or delete buffer
  ;; contents past the boundaries of the current prompt.
  (add-hook 'comint-exec-hook #'buffer-disable-undo)
  (defadvice! zenit--comint-enable-undo-a (process _string)
    :after #'comint-output-filter
    (with-current-buffer (process-buffer process)
      (when-let* ((start-marker comint-last-output-start))
        (when (and (< start-marker
                      (or (if process (process-mark process))
                          (point-max-marker)))
                   ;; Account for some of the IELM’s wilderness.
                   (eq (char-before start-marker) ?\n))
          (buffer-enable-undo)
          (setq buffer-undo-list nil)))))

  ;; Protect prompts from accidental modifications.
  (setq-default comint-prompt-read-only t)

  ;; Prior output in shell and comint shells (like ielm) should be read-only.
  ;; Otherwise, it's trivial to make edits in visual modes (like evil's or
  ;; term's term-line-mode) and leave the buffer in a half-broken state (which
  ;; you have to flush out with a couple RETs, which may execute the broken text
  ;; in the buffer),
  (defadvice! zenit--comint-protect-output-in-visual-modes-a (process _string)
    :after #'comint-output-filter
    ;; Adapted from
    ;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L33-L49
    (with-current-buffer (process-buffer process)
      (let ((start-marker comint-last-output-start)
            (end-marker (process-mark process)))
        ;; Account for some of the IELM’s wilderness.
        (when (and start-marker (< start-marker end-marker))
          (let ((inhibit-read-only t))
            ;; Make all past output read-only (disallow buffer modifications)
            (add-text-properties comint-last-input-start (1- end-marker) '(read-only t))
            ;; Disallow interleaving.
            (remove-text-properties start-marker (1- end-marker) '(rear-nonsticky))
            ;; Make sure that at `max-point' you can always append. Important
            ;; for bad REPLs that keep writing after giving us prompt (e.g.
            ;; sbt).
            (add-text-properties (1- end-marker) end-marker '(rear-nonsticky t))
            ;; Protect fence (newline of input, just before output).
            (when (eq (char-before start-marker) ?\n)
              (remove-text-properties (1- start-marker) start-marker '(rear-nonsticky))
              (add-text-properties (1- start-marker) start-marker '(read-only t))))))))

  ;; If the user is anywhere but the last prompt, typing should move them there
  ;; instead of unhelpfully spew read-only errors at them.
  (defun zenit--comint-move-cursor-to-prompt-h ()
    (and (eq this-command 'self-insert-command)
         comint-last-prompt
         (> (cdr comint-last-prompt) (point))
         (goto-char (cdr comint-last-prompt))))
  (eval-when-compile
    (declare-function zenit--comint-move-cursor-to-prompt-h nil))

  (add-hook! 'comint-mode-hook
    (defun zenit--comint-init-move-cursor-to-prompt-h ()
      (add-hook 'pre-command-hook #'zenit--comint-move-cursor-to-prompt-h
                nil t))))


(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))


(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar zenit--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff

  (add-hook! 'ediff-before-setup-hook
    (defun zenit-ediff-save-wconf-h ()
      (setq zenit--ediff-saved-wconf (current-window-configuration))))

  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun zenit-ediff-restore-wconf-h ()
      (when (window-configuration-p zenit--ediff-saved-wconf)
        (set-window-configuration zenit--ediff-saved-wconf)))))


(use-package! goto-addr
  :hook (text-mode . goto-address-mode)
  :hook (prog-mode . goto-address-prog-mode))


(use-package! hl-line
  ;; Highlights the current line
  :hook (zenit-first-buffer . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
      org-agenda-mode dired-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;   `global-hl-line-modes' _and_ so we can use `global-hl-line-mode', which
  ;;   users expect to control hl-line in Emacs.
  (defun +hl-line--turn-on-global-hl-line-mode ()
    "Turn on global `hl-line-mode' if conditions are met."
    (and (cond (hl-line-mode nil)
               ((null global-hl-line-modes) nil)
               ((eq global-hl-line-modes t))
               ((eq (car global-hl-line-modes) 'not)
                (not (derived-mode-p global-hl-line-modes)))
               ((apply #'derived-mode-p global-hl-line-modes)))
         (hl-line-mode +1)))
  (eval-when-compile
    (declare-function +hl-line--turn-on-global-hl-line-mode nil))

  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (+hl-line--turn-on-global-hl-line-mode))
    :group 'hl-line)
  (eval-when-compile
    (declare-function hl-line-mode-set-explicitly nil)
    (declare-function global-hl-line-mode-cmhh nil)
    (declare-function global-hl-line-mode-check-buffers nil)
    (declare-function global-hl-line-mode-enable-in-buffer nil)
    (declare-function global-hl-line-mode-enable-in-buffers nil))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar zenit--hl-line-mode nil)

  (add-hook! 'hl-line-mode-hook
    (defun zenit-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local zenit--hl-line-mode nil))))

  (add-hook! 'evil-visual-state-entry-hook
    (defun zenit-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local zenit--hl-line-mode t))))

  (add-hook! 'evil-visual-state-exit-hook
    (defun zenit-enable-hl-line-maybe-h ()
      (when zenit--hl-line-mode
        (hl-line-mode +1)))))


(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (zenit-first-buffer . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))


(use-package! paren
  ;; highlight matching delimiters
  :hook (zenit-first-buffer . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))


;;
;;; Third party packages

(use-package! nerd-icons
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))

;; Hide the mode line in completion popups and MAN pages because they serve
;; little purpose there, and is better hidden.
;;;###package hide-mode-line-mode
(add-hook! '(completion-list-mode-hook Man-mode-hook)
           #'hide-mode-line-mode)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package image
(setq image-animate-loop t)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp. I reduce it from it's default of 9 to reduce the
;; complexity of the font-lock keyword and hopefully buy us a few ms of
;; performance.
(setq rainbow-delimiters-max-face-count 4)


;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)


;;
;;; Theme & font

;; User themes should live in ~/.emacs.d/site-lisp/themes, not ~/.emacs.d
(setq custom-theme-directory (concat zenit-local-conf-dir "themes/"))

;; Third party themes add themselves to `custom-theme-load-path', but the themes
;; living in ~/.emacs.d/site-lisp/themes should always have priority.
(setq custom-theme-load-path
      (cons 'custom-theme-directory
            (delq 'custom-theme-directory custom-theme-load-path)))

(defun zenit-init-fonts-h (&optional reload)
  "Load `zenit-font', `zenit-serif-font', and `zenit-variable-pitch-font'."
  (let ((initialized-frames (unless reload (get 'zenit-font 'initialized-frames))))
    (dolist (frame (if reload (frame-list) (list (selected-frame))))
      (unless (member frame initialized-frames)
        (dolist (map `((default . ,zenit-font)
                       (fixed-pitch . ,zenit-font)
                       (fixed-pitch-serif . ,zenit-serif-font)
                       (variable-pitch . ,zenit-variable-pitch-font)))
          (condition-case e
              (when-let* ((face (car map))
                          (font (cdr map)))
                (when (display-multi-font-p frame)
                  (set-face-attribute face frame
                                      :width 'normal :weight 'normal
                                      :slant 'normal :font font))
                (custom-push-theme
                 'theme-face face 'user 'set
                 (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
                        (base-specs (or base-specs '((t nil))))
                        (attrs '(:family :foundry :slant :weight :height :width))
                        (new-specs nil))
                   (dolist (spec base-specs)
                     (let ((display (car spec))
                           (plist (copy-tree (nth 1 spec))))
                       (when (or (memq display '(t default))
                                 (face-spec-set-match-display display frame))
                         (dolist (attr attrs)
                           (setq plist (plist-put plist attr (face-attribute face attr)))))
                       (push (list display plist) new-specs)))
                   (nreverse new-specs)))
                (put face 'face-modified nil))
            (error
             (if (string-prefix-p "Font not available" (error-message-string e))
                 (signal 'zenit-font-error (list (font-get (cdr map) :family)))
               (signal (car e) (cdr e))))))
        (put 'zenit-font 'initialized-frames
             (cons frame (cl-delete-if-not #'frame-live-p initialized-frames))))))
  ;; Only do this once per session (or on `zenit/reload-fonts'); superfluous
  ;; `set-fontset-font' calls may segfault in some contexts.
  (when (or reload (not (get 'zenit-font 'initialized)))
    (when (fboundp 'set-fontset-font)  ; unavailable in emacs-nox
      (let* ((fn (zenit-rpartial #'member (font-family-list)))
             (symbol-font (or zenit-symbol-font
                              (cl-find-if fn zenit-symbol-fallback-font-families)))
             (emoji-font (or zenit-emoji-font
                             (cl-find-if fn zenit-emoji-fallback-font-families))))
        (when symbol-font
          (dolist (script '(symbol mathematical))
            (set-fontset-font t script symbol-font)))
        (when emoji-font
          (set-fontset-font t 'emoji emoji-font)
          ;; some characters in the Emacs symbol script are often covered by
          ;; emoji fonts
          (set-fontset-font t 'symbol emoji-font nil 'append)))
      ;; Nerd Fonts use these Private Use Areas
      (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
        (set-fontset-font t range "Symbols Nerd Font Mono")))
    (run-hooks 'after-setting-font-hook))
  (put 'zenit-font 'initialized t))

(defun zenit-init-theme-h (&rest _)
  "Load the theme specified by `zenit-theme' in FRAME."
  (dolist (th (ensure-list zenit-theme))
    (unless (custom-theme-enabled-p th)
      (if (custom-theme-p th)
          (enable-theme th)
        (load-theme th t)))))

(defadvice! zenit--detect-colorscheme-a (theme)
  "Add :kind \\='color-scheme to THEME if it doesn't have one.

Themes wouldn't call `provide-theme' unless they were a
color-scheme, so treat them as such. Also intended as a helper
for `zenit--theme-is-colorscheme-p'."
  :after #'provide-theme
  (with-memoization (plist-get (get theme 'theme-properties) :kind)
    'color-scheme))

(defun zenit--theme-is-colorscheme-p (theme)
  "Non-nil if THEME is a colorschema."
  (unless (memq theme '(nil user changed use-package))
    (if-let* ((kind (plist-get (get theme 'theme-properties) :kind)))
        ;; Some newer themes announce that they are colorschemes. Also, we've
        ;; advised `provide-theme' (only used by colorschemes) to give these
        ;; themes this property (see `zenit--detect-colorscheme-a').
        (eq kind 'color-scheme)
      ;; HACK: If by some chance a legit (probably very old) theme isn't using
      ;;   `provide-theme' (ugh), fall back to this hail mary heuristic to
      ;;   detect colorscheme themes:
      (let ((feature (get theme 'theme-feature)))
        (and
         ;; Colorschemes always have a theme-feature (possible to define them
         ;; without one with `custom-declare-theme' + a nil second argument):
         feature
         ;; ...and they always end in -theme (this is hardcoded into `deftheme'
         ;; and others in Emacs' theme API).
         (string-suffix-p "-theme" (symbol-name feature))
         ;; ...and any theme (deftheme X) will have a corresponding `X-theme'
         ;; package loaded when it's enabled.
         (featurep feature))))))

(add-hook! 'enable-theme-functions :depth -90
  (defun zenit-enable-theme-h (theme)
    "Record themes and trigger `zenit-load-theme-hook'."
    (when (zenit--theme-is-colorscheme-p theme)
      (ring-insert (with-memoization (get 'zenit-theme 'history) (make-ring 8))
                   (copy-sequence custom-enabled-themes))
      ;; Functions in `zenit-load-theme-hook' may trigger face recalculations,
      ;; which can be contaminated by buffer-local face remaps (e.g. by
      ;; `mixed-pitch-mode'); this prevents that contamination:
      (with-temp-buffer
        (let ((enable-theme-functions
               (remq 'zenit-enable-theme-h enable-theme-functions)))
          (zenit-run-hooks 'zenit-load-theme-hook))))))

(add-hook! 'after-make-frame-functions :depth -90
  (defun zenit-fix-frame-color-parameters-h (f)
    ;; HACK: Some window systems produce new frames (after the initial one) with
    ;;   incorrect color parameters (black).
    (when (display-graphic-p f)
      (letf! (defun invalid-p (color)
               (or (equal color "black")
                   (string-prefix-p "unspecified-" color)))
        (pcase-dolist (`(,param ,fn ,face)
                       '((foreground-color face-foreground default)
                         (background-color face-background default)
                         (cursor-color face-background cursor)
                         (border-color face-background border)
                         (mouse-color face-background mouse)))
          (when-let* ((color (frame-parameter f param))
                      ((invalid-p color))
                      (color (funcall fn face nil t))
                      ((not (invalid-p color))))
            (set-frame-parameter f param color)))))))


;;
;;; Bootstrap

(defun zenit-init-ui-h (&optional _)
  "Initialize Emacs' user interface.

Apply all its advices and hooks. These should be done as late as
possible, as to avoid/minimize prematurely triggering hooks
during startup."
  (zenit-run-hooks 'zenit-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'zenit-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'zenit-highlight-non-default-indentation-h 'append)

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (push '(buffer-predicate . zenit-buffer-frame-predicate) default-frame-alist)

  ;; Initialize `zenit-switch-window-hook' and `zenit-switch-frame-hook'
  (add-function :after after-focus-change-function #'zenit-run-switch-frame-hooks-fn)
  (add-hook 'window-selection-change-functions #'zenit-run-switch-window-hooks-h)
  ;; Initialize `zenit-switch-buffer-hook'
  (add-hook 'window-buffer-change-functions #'zenit-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the
  ;; server.
  (add-hook 'server-switch-hook #'zenit-run-switch-buffer-hooks-h))

;; Apply fonts and theme
(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'zenit-init-fonts-h -100)
  (add-hook hook #'zenit-init-theme-h -90))

;; Init UI late, but not too late. Its impact on startup time seems to vary
;; wildly depending on exact placement. `window-setup-hook' appears to be the
;; sweet spot.
(add-hook 'window-setup-hook #'zenit-init-ui-h -100)


;;
;;; Fixes/hacks

;; At the time of writing, the `customize' interface is not really supported. It
;; is better to configure via setting variables in ~/.emacs.d/site-lisp.
(dolist (sym '(customize-option customize-browse customize-group customize-face
               customize-rogue customize-saved customize-apropos
               customize-changed customize-unsaved customize-variable
               customize-set-value customize-customized customize-set-variable
               customize-apropos-faces customize-save-variable
               customize-apropos-groups customize-apropos-options
               customize-changed-options customize-save-customized))
  (put sym 'disabled "`customize' not supported, configure Emacs from ~/.emacs.d/site-lisp/config.el instead"))
(put 'customize-themes 'disabled "Set `zenit-theme' or use `load-theme' in ~/.emacs.d/site-lisp/config.el instead")

(after! whitespace
  (defun zenit--in-parent-frame-p ()
    "`whitespace-mode' inundates child frames with whitespace
markers, so disable it to fix all that visual noise."
    (null (frame-parameter nil 'parent-frame)))
  (eval-when-compile
    (declare-function zenit--in-parent-frame-p nil))
  (add-function :before-while whitespace-enable-predicate #'zenit--in-parent-frame-p))

(provide 'zenit-ui)
