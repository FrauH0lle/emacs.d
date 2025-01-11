;; lang/ess/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'el-patch)
  (require 'xterm-color))

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

(el-patch-cl-defmacro xterm-color--with-SGR-constants (&body body)
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

(el-patch-defsubst xterm-color--dispatch-SGR (SGR-list)
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

(el-patch-defmacro xterm-color--with-ANSI-macro-helpers (&rest body)
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
