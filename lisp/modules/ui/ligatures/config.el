;; ui/ligatures/config.el -*- lexical-binding: t; -*-

(defvar +ligatures-extra-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    :quote         "“"
    :quote_end     "”"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    :list          "𝕃"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :union         "⋃"
    :intersect     "∩"
    :diff          "∖"
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Maps identifiers to symbols, recognized by `set-ligatures'.

This should not contain any symbols from the Unicode Private
Area! There is no universal way of getting the correct symbol as
that area varies from font to font.")

(defvar +ligatures-alist
  '((prog-mode
     "|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://")
    (t))
  "A alist of ligatures to enable in specific modes.")

(defvar +ligatures-extra-alist '((t))
  "A map of major modes to symbol lists.

Used by `prettify-symbols-alist'.")

(defvar +ligatures-extras-in-modes t
  "List of major modes where extra ligatures should be enabled.

Extra ligatures are mode-specific substituions, defined in
`+ligatures-extra-symbols' and assigned with `set-ligatures!'.
This variable controls where these are enabled.

  If t, enable it everywhere (except `fundamental-mode').
  If the first element is \\='not, enable it in any mode besides
  what is listed.
  If nil, don't enable these extra ligatures anywhere (though
  it's more efficient to remove the `+extra' flag from the :ui
  ligatures module instead).")

(defun +ligatures--enable-p (modes)
  "Return t if ligatures should be enabled in this buffer
 depending on MODES."
  (unless (eq major-mode 'fundamental-mode)
    (or (eq modes t)
        (if (eq (car modes) 'not)
            (not (apply #'derived-mode-p (cdr modes)))
          (apply #'derived-mode-p modes)))))

(defun +ligatures-init-extra-symbols-h ()
  "Set up `prettify-symbols-mode' for the current buffer.

Overwrites `prettify-symbols-alist' and activates
`prettify-symbols-mode' if (and only if) there is an associated
entry for the current major mode (or a parent mode) in
`+ligatures-extra-alist' AND the current mode (or a parent mode)
isn't disabled in `+ligatures-extras-in-modes'."
  (when after-init-time
    (when-let*
        (((+ligatures--enable-p +ligatures-extras-in-modes))
         (symbols
          (if-let ((symbols (assq major-mode +ligatures-extra-alist)))
              (cdr symbols)
            (cl-loop for (mode . symbols) in +ligatures-extra-alist
                     if (derived-mode-p mode)
                     return symbols))))
      (setq prettify-symbols-alist
            (append symbols
                    ;; Don't overwrite global defaults
                    (default-value 'prettify-symbols-alist)))
      (when (bound-and-true-p prettify-symbols-mode)
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))

(defun +ligatures/toggle ()
  "Toggle ligatures for the current buffer."
  (interactive)
  (cond ((or (bound-and-true-p ligature-mode)
             (bound-and-true-p prettify-symbols-mode))
         (ligature-mode -1)
         (prettify-symbols-mode -1))
        (t
         (+ligatures-init-extra-symbols-h)
         (ligature-mode +1))))


;;
;;; Bootstrap

;;;###package prettify-symbols
;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(eval-when! (modulep! +auto)
  (eval-when! (modulep! +extra)
    ;; Lisp modes offer their own defaults for `prettify-symbols-mode' (just a
    ;; lambda symbol substitution), but this might be unexpected if the user
    ;; enables +extra but has unset `+ligatures-extra-symbols'.
    (setq lisp-prettify-symbols-alist nil)
    (add-hook 'after-change-major-mode-hook #'+ligatures-init-extra-symbols-h)))

(cond
 ;; The emacs-mac build of Emacs appears to have built-in support for ligatures,
 ;; using the same composition-function-table method
 ;; https://bitbucket.org/mituharu/emacs-mac/src/26c8fd9920db9d34ae8f78bceaec714230824dac/lisp/term/mac-win.el?at=master#lines-345:805
 ;; so use that instead if this module is enabled.
 ((when zenit--system-macos-p
    (fboundp 'mac-auto-operator-composition-mode))
  (add-hook 'zenit-init-ui-hook #'mac-auto-operator-composition-mode 'append))

 ;; This module does not support Emacs 27 and less, but if we still try to
 ;; enable ligatures, it will end up in catastrophic work-loss errors, so we
 ;; leave the check here for safety.
 ((and (> emacs-major-version 27)
       (or (featurep 'ns)
           (featurep 'harfbuzz))
       (featurep 'composite))   ; Emacs loads `composite' at startup

  (use-package! ligature
    :commands ligature-mode-turn-on
    :config
    (dolist (lig +ligatures-alist)
      (ligature-set-ligatures (car lig) (cdr lig))))

  (eval-when! (modulep! +auto)
    (add-hook! 'zenit-init-ui-hook :append
      (defun +ligature-enable-globally-h ()
        "Enables ligature checks globally in all buffers.
You can also do it per mode with `ligature-mode'."
        (global-ligature-mode t))))))
