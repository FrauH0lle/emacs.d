;; lang/ess/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))

(defvar +ess-R-remote-host nil
  "Host to connect to, given as a string.
Should be set in a local variable.")

(defvar +ess-R-remote-session nil
  "Name of the R session to be created, given as a string.
Should be set in a local variable.")

(defvar +ess-R-remote-cmds nil
  "Commands to be executed on the host before R is launched,
given as a list of strings. Should be set in a local
variable.")


;;
;;; Packages

(use-package! xterm-color
  :defer t
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  ;; Fix wrong broken color ouput in comint buffer. See
  ;; https://github.com/emacs-ess/ESS/issues/1193
  (add-hook 'inferior-ess-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)))
  :config/el-patch
  ;; PATCH 2024-07-30: The `inferior-ess-r-mode' can produce unreadable output
  ;;   for tidyverse commands. See https://github.com/emacs-ess/ESS/issues/1193.
  ;;   Since Emacs 29+, this seems to be less of an issue, however using
  ;;   `xterm-color' seems to solve it.
  ;;
  ;;  The patches below are taken from:
  ;;  - https://github.com/lionel-/xterm-color/commit/d63452ea8f16632179bd322c21bd804603fe4286
  ;;    - Directly from on of the ESS maintainers
  (defcustom xterm-color-use-bold nil
    "If non-nil, render bold attribute as bold."
    :type 'boolean
    :group 'xterm-color)

  (cl-defmacro xterm-color--with-SGR-constants (&body body)
    (declare (indent defun))
    `(cl-symbol-macrolet
         ((+bright+           1)
          (+italic+           2)
          (+underline+        4)
          (+strike-through+   8)
          (+negative+        16)
          (+frame+           32)
          (+overline+        64)
          (el-patch-add
            (+bold+            128)))
       ,@body))

  (defsubst xterm-color--dispatch-SGR (SGR-list)
    "Update state machine based on SGR-LIST (list of SGR attributes /integers)."
    (xterm-color--create-SGR-table (elem SGR-list)
      (:match (0)  (reset!))                              ; RESET everything
      (:match ((<= 30 elem 37)) (set-f! (- elem 30)))     ; ANSI FG color
      (:match ((<= 40 elem 47)) (set-b! (- elem 40)))     ; ANSI BG color
      (:match (39) (set-f!   nil))                        ; RESET FG color (switch to default)
      (:match (49) (set-b!   nil))                        ; RESET BG color (switch to default)
      (:match (1)  (set-a!   (el-patch-swap +bright+ (if xterm-color-use-bold +bold+ +bright+))))
      (:match (2)  (unset-a! +bright+))
      (:match (22) (unset-a! (el-patch-swap +bright+ (if xterm-color-use-bold +bold+ +bright+))))

      (:match ((and (eq 38 (cl-first SGR-list))
                    (eq 2 (cl-second SGR-list)))          ; Truecolor (24-bit) FG color
               :skip 5)
              (when xterm-color--support-truecolor
                (if-let ((r (cl-third SGR-list))
                         (g (cl-fourth SGR-list))
                         (b (cl-fifth SGR-list)))
                    (if (or (> r 255) (> g 255) (> b 255))
                        (xterm-color--message "SGR 38;2;%s;%s;%s exceeds range"
                                              r g b)
                      (set-truecolor! r g b xterm-color--current-fg))
                  (xterm-color--message "SGR 38;2;%s;%s;%s error, expected 38;2;R;G;B"
                                        r g b))))
      (:match ((and (eq 38 (cl-first SGR-list))
                    (eq 5 (cl-second SGR-list)))
               :skip 3)                                   ; XTERM 256 FG color
              (if-let ((color (cl-third SGR-list)))
                  (if (> color 255)
                      (xterm-color--message "SGR 38;5;%s exceeds range" color)
                    (set-f! color))
                (xterm-color--message "SGR 38;5;%s error, expected 38;5;COLOR"
                                      color)))
      (:match ((and (eq 48 (cl-first SGR-list))
                    (eq 2 (cl-second SGR-list)))          ; Truecolor (24-bit) BG color
               :skip 5)
              (when xterm-color--support-truecolor
                (if-let ((r (cl-third SGR-list))
                         (g (cl-fourth SGR-list))
                         (b (cl-fifth SGR-list)))
                    (if (or (> r 255) (> g 255) (> b 255))
                        (xterm-color--message "SGR 48;2;%s;%s;%s exceeds range"
                                              r g b)
                      (set-truecolor! r g b xterm-color--current-bg))
                  (xterm-color--message "SGR 48;2;%s;%s;%s error, expected 48;2;R;G;B"
                                        r g b))))
      (:match ((and (eq 48 (cl-first SGR-list))
                    (eq 5 (cl-second SGR-list)))
               :skip 3)                                   ; XTERM 256 BG color
              (if-let ((color (cl-third SGR-list)))
                  (if (> color 255)
                      (xterm-color--message "SGR 48;5;%s exceeds range" color)
                    (set-b! color))
                (xterm-color--message "SGR 48;5;%s error, expected 48;5;COLOR"
                                      color)))
      (:match ((<= 90 elem 97))                           ; AIXTERM hi-intensity FG
              ;; Rather than setting bright, which would be wrong,
              ;; rescale color to fall within 8-15 so that it gets
              ;; mapped to xterm-color-names-bright by xterm-color-256
              (set-f! (- elem 82)))
      ;; Same for BG, rescale to 8-15
      (:match ((<= 100 elem 107)) (set-b! (- elem 92)))   ; AIXTERM hi-intensity BG
      (:match (51) (set-a!   +frame+))
      (:match (53) (set-a!   +overline+))
      (:match (54) (unset-a! +frame+))
      (:match (55) (unset-a! +overline+))
      (:match (4)  (set-a!   +underline+))
      (:match (24) (unset-a! +underline+))
      (:match (3)  (set-a!   +italic+))
      (:match (23) (unset-a! +italic+))
      (:match (9)  (set-a!   +strike-through+))
      (:match (29) (unset-a! +strike-through+))
      (:match (7)  (set-a!   +negative+))
      (:match (27) (unset-a! +negative+))))

  (defmacro xterm-color--with-ANSI-macro-helpers (&rest body)
    (declare (indent defun))
    `(xterm-color--with-SGR-constants
       (cl-symbol-macrolet ((fg           xterm-color--current-fg)
                            (bg           xterm-color--current-bg)
                            (attrs        xterm-color--attributes)
                            (el-patch-add (bold         xterm-color-use-bold))
                            (bold-bright  xterm-color-use-bold-for-bright))
         (cl-macrolet
             ((out! (x)            `(push ,x result))
              (push-char! (c)      `(push ,c xterm-color--char-list))
              (push-csi! (c)       `(push ,c xterm-color--CSI-list))
              (state! (s)          `(setq state ,s))
              (graphics? ()        `(or fg bg (/= attrs 0)))
              (has? (attr)         `(/= (logand ,attr attrs) 0))
              (fmt-24bit (color)   `(format "#%06x" ,color))
              (fmt-256 (color)     `(xterm-color-256 ,color))
              ;; Unpacks a packed truecolor value (as stored in
              ;; `xterm-color--current-fg' and `xterm-color--current-fg'.
              (unpack (color)      `(ash ,color -9))
              ;; To avoid hash collisions, a different packing scheme is used
              ;; for hash table keys. It can encode two colors (foreground
              ;; and background) that can either be truecolor 24bit or XTerm 256
              ;; color 8bit. XTerm 256 color values subsume ANSI colors, a
              ;; separate encoding scheme is not needed.
              ;;
              ;; The scheme used also accounts for the combination of a truecolor
              ;; with an XTerm 256 color as part of the same hashed entry. Since
              ;; two different hash tables are used to work around 32bit Emacs
              ;; limited integer range, two packing schemes are needed:
              ;;
              ;; High<         25 bits       >Low
              ;; ATTR[7 bits]BG[9 bits]FG[9 bits] where BG and FG are each
              ;; encoded as the 8bit color value shifted left by 1 and combined
              ;; with a flag bit which is set when the color is present.
              ;;
              ;; High<         59 bits       >Low
              ;; ATTR[7 bits]BG[26 bits]FG[26 bits] where BG and FG are each
              ;; encoded as the 24bit (RGB) or 8bit color value shifted left by
              ;; 2 and combined with 2 flag bits that are set when the value
              ;; is 24bit (high bit) and when the color is present (low bit).
              (pack-256 (color)    `(if ,color (logior (ash ,color 1) 1) 0))
              (pack-24bit (color)  `(if ,color
                                        (if (> ,color 255)
                                            (logior (ash (unpack ,color) 2) 3)
                                          (logior (ash ,color 2) 1))
                                      0))
              ;; If at least one of foreground / background color is a 24bit
              ;; truecolor value: Second packing scheme with
              ;; `xterm-color--truecolor-face-cache' is used.
              ;;
              ;; Every other case, including when no colors are present:
              ;; First packing scheme with `xterm-color--face-cache' is used.
              (pack-key-into (k)   `(cond ((or (and fg (> fg 255))
                                               (and bg (> bg 255)))
                                           ;; At least one truecolor 24bit value
                                           (setq ,k (logior (ash attrs 52)
                                                            (ash (pack-24bit bg) 26)
                                                            (pack-24bit fg)))
                                           xterm-color--truecolor-face-cache)
                                          (t ;; No truecolor 24bit value
                                           (setq ,k (logior (ash attrs 18)
                                                            (ash (pack-256 bg) 9)
                                                            (pack-256 fg)))
                                           xterm-color--face-cache)))
              (face! (k v)         `(setq plistf (plist-put plistf ,k ,v)))
              (make-color-fg ()    `(if (and bold-bright
                                             (< fg 256)
                                             (or (has? +bright+) (<= 8 fg 15)))
                                        (progn (face! :weight 'bold)
                                               (face! :foreground
                                                      (fmt-256 (if (<= 8 fg) (- fg 8) fg))))
                                      (face! :foreground
                                             (if (> fg 255)
                                                 (fmt-24bit (unpack fg))
                                               (fmt-256 (if (and (<= fg 7) (has? +bright+))
                                                            (+ fg 8)
                                                          fg))))))
              (make-color-bg ()    `(face! :background (cond ((> bg 255) (fmt-24bit (unpack bg)))
                                                             (t (fmt-256 bg)))))
              (make-face ()        `(let* (k
                                           (table (pack-key-into k)))
                                      (or (gethash k table)
                                          (let (plistf)
                                            (el-patch-add (when (has? +bold+)           (face! :weight 'bold)))
                                            (when (has? +italic+)         (face! :slant 'italic))
                                            (when (has? +underline+)      (face! :underline t))
                                            (when (has? +strike-through+) (face! :strike-through t))
                                            (when (has? +negative+)       (face! :inverse-video t))
                                            (when (has? +overline+)       (face! :overline t))
                                            (when (has? +frame+)          (face! :box t))
                                            (cond (fg (make-color-fg))
                                                  (t (when (and bold-bright (has? +bright+))
                                                       (face! :weight 'bold))))
                                            (when bg (make-color-bg))
                                            (puthash k plistf table)))))
              (maybe-fontify ()    '(when xterm-color--char-list
                                      (let ((s (concat (nreverse xterm-color--char-list))))
                                        (when (and xterm-color-render (graphics?))
                                          (add-text-properties
                                           0 (length s)
                                           (list 'xterm-color t
                                                 (if font-lock-mode 'font-lock-face 'face)
                                                 (make-face))
                                           s))
                                        (out! s))
                                      (setq xterm-color--char-list nil))))
           ,@body))))
  :config
  (setq! xterm-color-use-bold t))

(use-package! ess
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.[Ss][s][c]\\'"   . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]md\\'"       . poly-markdown+r-mode)
         ("\\.[sS]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R stata julia SAS ess-julia-mode)
  :init
  ;; Support Juila only if no dedicated module is used.
  (eval-unless! (modulep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.[Jj][Ll]\\'" . ess-julia-mode)))
  ;; Tree-sitter support
  (eval-when! (modulep! +tree-sitter)
    (add-hook 'ess-r-mode-local-vars-hook #'tree-sitter! 'append))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (not (modulep! :checkers syntax))
        ess-nuke-trailing-whitespace-p t
        ess-style 'DEFAULT
        ess-history-directory (expand-file-name "ess-history/" zenit-cache-dir))

  ;; Set fontification
  ;; ESS buffer
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op%       . t)
          (ess-fl-keyword:fun-calls    . t)
          (ess-fl-keyword:numbers      . t)
          (ess-fl-keyword:operators    . t)
          (ess-fl-keyword:delimiters   . t)
          (ess-fl-keyword:=            . t)
          (ess-R-fl-keyword:F&T        . t)))

  ;; iESS buffer
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt      . t)
          (ess-R-fl-keyword:keywords    . t)
          (ess-R-fl-keyword:constants   . t)
          (ess-R-fl-keyword:modifiers   . t)
          (ess-R-fl-keyword:messages    . t)
          (ess-R-fl-keyword:fun-defs    . t)
          (ess-R-fl-keyword:assign-ops  . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls     . t)
          (ess-fl-keyword:numbers       . t)
          (ess-fl-keyword:operators     . t)
          (ess-fl-keyword:delimiters    . t)
          (ess-fl-keyword:=             . t)
          (ess-R-fl-keyword:F&T         . t)))

  (eval-when! (modulep! :tools lookup)
    (set-docsets! 'ess-r-mode :docsets "R")
    (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
                          :documentation #'ess-display-help-on-object))
  (eval-when! (modulep! :tools eval)
    (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
    (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
    (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
    (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go))

  (eval-when! (modulep! :editor evil)
    (set-evil-initial-state! 'ess-r-help-mode 'normal))

  ;; HACK If `+default-want-RET-continue-comments' is true, comments are
  ;;      continued on RET. But ess-r-mode doesn't have a sane
  ;;      `comment-line-break-function', so...
  (setq-hook! 'ess-r-mode-hook
    comment-line-break-function nil)

  ;; LSP
  (eval-when! (and (modulep! :tools lsp)
                   (modulep! :lang ess +lsp))
    (add-hook! 'ess-r-mode-local-vars-hook
      (defun +ess-lsp-init-maybe-h ()
        "Use LSP mode if the buffer is not a remote."
        (unless (file-remote-p default-directory)
          (lsp!)))))

  ;; Popup rules
  (eval-when! (modulep! :ui popup)
    (after! ess-r-mode
      (set-popup-rule! "^\\*R" :side 'bottom :height 0.33 :width 0.5 :quit nil)
      (set-popup-rule! "^\\*R dired*" :side 'right :size 0.25 :height 0.5 :vslot 99 :slot 1
        :select nil :quit nil))
    (after! ess-help
      (set-popup-rule! "^\\*help.R.*" :side 'right :size 0.25 :height 0.5 :vslot 100 :slot 1
        :select t :quit t :transient t)))

  ;; Workspaces integration
  (eval-when! (modulep! :ui workspaces)
    (after! ess-r-mode
      (defun +ess-r-bookmark-make-record ()
        "Create a bookmark for the current iESS buffer."
        `(,(format "ess-r-%s"
                   (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory default-directory))))
          (location . ,default-directory)
          (ess-r-wd . ,(ess-get-working-directory t))
          (handler . +ess-r-bookmark-jump)))

      (defun +ess-r-bookmark-jump (bookmark)
        "Default bookmark handler for iESS buffers."
        (let ((ess-ask-for-ess-directory nil)
              (default-directory (bookmark-prop-get bookmark 'location))
              (wd (bookmark-prop-get bookmark 'ess-r-wd)))
          (R)
          (ess-set-working-directory wd t)))

      (add-hook! 'inferior-ess-r-mode-hook
        (setq-local bookmark-make-record-function #'+ess-r-bookmark-make-record))))

  ;; REPL
  ;; Use smartparens in iESS
  (add-hook! 'inferior-ess-mode-hook #'smartparens-mode)

  ;; Use evil insert state in iEES
  (eval-when! (modulep! :editor evil)
    (after! ess-r-mode
      (set-evil-initial-state! 'inferior-ess-mode 'insert)))

  (add-hook! 'inferior-ess-mode-hook
    (defun +ess-fix-read-only-inferior-ess-mode-h ()
      "Fixes a bug when `comint-prompt-read-only' in non-nil.
See URL `https://github.com/emacs-ess/ESS/issues/300'."
      (setq-local comint-use-prompt-regexp nil)
      (setq-local inhibit-field-text-motion nil)))

  ;; Make the REPL buffer more responsive.
  (setq-hook! 'inferior-ess-mode-hook
    comint-scroll-to-bottom-on-input t
    comint-scroll-to-bottom-on-output t
    comint-move-point-for-output t)

  (add-hook! 'zenit-real-buffer-functions
    (defun +ess-inferior-buffer-p (buf)
      "Returns non-nil if BUF is a `inferior-ess' buffer."
      (with-current-buffer buf (derived-mode-p 'inferior-ess-mode))))

  ;; Save history when killing the ess inferior buffer. See
  ;; https://github.com/emacs-ess/ESS/issues/970
  (defun +ess-kill-proc-before-buffer-h ()
    ;; The .Rhistory file needs to exist beforehand
    (unless (file-exists-p comint-input-ring-file-name)
      (make-empty-file comint-input-ring-file-name))
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (and (derived-mode-p 'comint-mode)
             (comint-write-input-ring))
        (delete-process proc))))

  (defun +ess-run-kill-proc-maybe-h ()
    (ignore-errors
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (and (derived-mode-p 'inferior-ess-mode)
               (+ess-kill-proc-before-buffer-h))))))

  (defun +ess-comint-h ()
    (add-hook 'kill-buffer-hook #'+ess-kill-proc-before-buffer-h nil t)
    (add-hook 'kill-emacs-hook #'+ess-run-kill-proc-maybe-h nil))

  (add-hook 'comint-mode-hook #'+ess-comint-h)

  (use-package! ess-rdired
    :defer t
    :config/el-patch
    ;; PATCH `ess-rdired'
    (defun ess-rdired-refresh ()
      "Refresh the `ess-rdired' buffer."
      (let* ((buff (get-buffer-create ess-rdired-buffer))
             (proc-name (buffer-local-value 'ess-local-process-name buff))
             (proc (get-process proc-name))
             (out-buff (get-buffer-create " *ess-rdired-output*"))
             text)
        (when (and proc-name proc
                   (not (process-get proc 'busy)))
          (ess--foreground-command ess-rdired-objects out-buff nil nil nil proc)
          (with-current-buffer out-buff
            (goto-char (point-min))
            ;; Delete two lines. One filled with +'s from R's prompt
            ;; printing, the other with the header info from the data.frame
            (el-patch-remove
              (delete-region (point-min) (1+ (line-end-position 2)))
              (setq text (split-string (buffer-string) "\n" t "\n"))
              (erase-buffer))
            (el-patch-add
              (when (> (count-lines (point-min) (point-max)) 2)
                (delete-region (point-min) (1+ (line-end-position 2)))
                (setq text (split-string (buffer-string) "\n" t "\n"))
                (erase-buffer))))
          (with-current-buffer buff
            (setq tabulated-list-entries
                  (mapcar #'ess-rdired--tabulated-list-entries text))
            (let ((entry (tabulated-list-get-id))
                  (col (current-column)))
              (tabulated-list-print)
              (while (not (equal entry (tabulated-list-get-id)))
                (forward-line))
              (move-to-column col)))))))

  ;; Keybinds
  (map!
   ;; REPL
   (:map inferior-ess-mode-map
    :i "C->" (cmd! (insert " %>% "))
    :i "M--" #'ess-cycle-assign
    (:when (modulep! :completion vertico)
      :i "C-r" #'consult-history)
    :n "RET" #'+ess/goto-end-of-prompt)
   (:map ess-mode-map
    :n [C-return] #'ess-eval-line-and-step
    :i "M--" #'ess-cycle-assign
    :i "C->" (cmd! (insert " %>% ")))
   (:after ess-help
           (:map ess-help-mode-map
            :n "q"  #'kill-current-buffer
            :n "Q"  #'ess-kill-buffer-and-go
            :n "K"  #'ess-display-help-on-object
            :n "go" #'ess-display-help-in-browser
            :n "gO" #'ess-display-help-apropos
            :n "gv" #'ess-display-vignettes
            :m "]]" #'ess-skip-to-next-section
            :m "[[" #'ess-skip-to-previous-section)
           (:map ess-doc-map
                 "h"    #'ess-display-help-on-object
                 "p"    #'ess-R-dv-pprint
                 "t"    #'ess-R-dv-ctable
                 [up]   #'comint-next-input
                 [down] #'comint-previous-input
                 [C-return] #'ess-eval-line))
   (:localleader
    :map ess-mode-map
    "," #'ess-eval-region-or-function-or-paragraph-and-step
    "'" #'R
    "s" #'ess-switch-to-inferior-or-script-buffer
    "S" #'ess-switch-process
    "B" #'ess-eval-buffer-and-go
    "b" #'ess-eval-buffer
    "d" #'ess-eval-region-or-line-and-step
    "D" #'ess-eval-function-or-paragraph-and-step
    "L" #'ess-eval-line-and-go
    "l" #'ess-eval-line
    "R" #'ess-eval-region-and-go
    "r" #'ess-eval-region
    "F" #'ess-eval-function-and-go
    "f" #'ess-eval-function
    ;; predefined keymaps
    "h" #'ess-doc-map
    "x" #'ess-extra-map
    "p" #'ess-r-package-dev-map
    "v" #'ess-dev-map
    ;; noweb
    (:prefix ("c" . "noweb")
             "C" #'ess-eval-chunk-and-go
             "c" #'ess-eval-chunk
             "d" #'ess-eval-chunk-and-step
             "m" #'ess-noweb-mark-chunk
             "N" #'ess-noweb-previous-chunk
             "n" #'ess-noweb-next-chunk))))


(use-package! ess-R-data-view
  :commands
  ess-R-dv-ctable
  ess-R-dv-pprint
  :config
  (map!
   :map ess-doc-map
   "h" #'ess-display-help-on-object
   "p" #'ess-R-dv-pprint
   "t" #'ess-R-dv-ctable))


(use-package! poly-R
  :mode ("\\.[rR]md$" . poly-markdown+r-mode)
  :config
  (eval-when! (modulep! :editor snippets)
    (set-tempel-minor-mode! 'poly-markdown+r-mode)))
