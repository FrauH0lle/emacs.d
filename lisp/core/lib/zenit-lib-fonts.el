;; lisp/core/lib/zenit-lib-fonts.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `zenit-ui'
(declare-function zenit-init-fonts-h "zenit-ui" (&optional reload))


;;;###autoload
(defvar zenit-font-increment 2
  "How many steps to increase the font size each time
`zenit/increase-font-size' or `zenit/decrease-font-size' are
invoked.")

;;;###autoload
(defvar zenit-big-font nil
  "The font to use for `zenit-big-font-mode'.
If nil, `zenit-font' will be used, scaled up by
`zenit-big-font-increment'. See `zenit-font' for details on
acceptable values for this variable.")

;;;###autoload
(defvar zenit-big-font-increment 4
  "How many steps to increase the font size (with `zenit-font'
as the base) when `zenit-big-font-mode' is enabled and
`zenit-big-font' is nil.")


;;
;;; Library

(cl-deftype font () '(satisfies fontp))
;;;###autoload
(defun zenit-normalize-font (font)
  "Return FONT as a normalized font spec.
The font will be normalized (i.e. :weight, :slant, and :width
will set to \\='normal if not specified) before it is converted.
FONT can be a `font-spec', a font object, an XFT font string, or
an XLFD font string."
  (cl-check-type font (or font string vector))
  (when (and (stringp font)
             (string-prefix-p "-" font))
    (setq font (x-decompose-font-name font)))
  (let* ((font
          (cond ((stringp font)
                 (dolist (prop '("weight" "slant" "width") (aref (font-info font) 0))
                   (unless (string-match-p (format ":%s=" prop) font)
                     (setq font (concat font ":" prop "=normal")))))
                ((fontp font)
                 (dolist (prop '(:weight :slant :width) (font-xlfd-name font))
                   (unless (font-get font prop)
                     (font-put font prop 'normal))))
                ((vectorp font)
                 (dolist (i '(1 2 3) (x-compose-font-name font))
                   (unless (aref font i)
                     (aset font i "normal"))))))
         (font (x-resolve-font-name font))
         (font (font-spec :name font)))
    (unless (font-get font :size)
      (font-put font :size
                (font-get (font-spec :name (face-font 'default))
                          :size)))
    font))

;;;###autoload
(defun zenit-adjust-font-size (increment &optional fixed-size-p font-alist)
  "Increase size of font in FRAME by INCREMENT.
If FIXED-SIZE-P is non-nil, treat INCREMENT as a font size,
rather than a scaling factor. FONT-ALIST is an alist give
temporary values to certain font variables, like `zenit-font' or
`zenit-variable-pitch-font'. e.g.

  `((zenit-font . ,(font-spec :family \"Sans Serif\" :size 12)))

Doesn't work in terminal Emacs."
  (unless (display-multi-font-p)
    (user-error "Cannot resize fonts in terminal Emacs"))
  (condition-case-unless-debug e
      (let (changed)
        (dolist (sym '((zenit-font . default)
                       (zenit-serif-font . fixed-pitch-serif)
                       (zenit-variable-pitch-font . variable-pitch))
                     (when changed
                       (zenit-init-fonts-h 'reload)
                       t))
          (cl-destructuring-bind (var . face) sym
            (if (null increment)
                (when (get var 'initial-value)
                  (set var (get var 'initial-value))
                  (put var 'initial-value nil)
                  (setq changed t))
              (let* ((original-font (or (symbol-value var)
                                        (face-font face t)
                                        (with-temp-buffer (face-font face))))
                     (font (zenit-normalize-font original-font))
                     (dfont
                      (or (if-let* ((remap-font (alist-get var font-alist))
                                    (remap-xlfd (zenit-normalize-font remap-font)))
                              remap-xlfd
                            (purecopy font))
                          (error "Could not decompose %s font" var))))
                (let* ((step      (if fixed-size-p 0 (* increment zenit-font-increment)))
                       (orig-size (font-get font :size))
                       (new-size  (if fixed-size-p increment (+ orig-size step))))
                  (cond ((<= new-size 0)
                         (error "`%s' font is too small to be resized (%d)" var new-size))
                        ((= orig-size new-size)
                         (user-error "Could not resize `%s' for some reason" var))
                        ((setq changed t)
                         (unless (get var 'initial-value)
                           (put var 'initial-value original-font))
                         (font-put dfont :size new-size)
                         (set var dfont)))))))))
    (error
     (ignore-errors (zenit-adjust-font-size nil))
     (signal (car e) (cdr e)))))


;;
;;; Commands

;;;###autoload
(defun zenit/reload-font ()
  "Reload your fonts, if they're set.
See `zenit-init-fonts-h'."
  (interactive)
  (zenit-init-fonts-h 'reload))

;;;###autoload
(defun zenit/increase-font-size (count &optional increment)
  "Enlargens the font size across the current and child frames."
  (interactive "p")
  (zenit-adjust-font-size (* count (or increment zenit-font-increment))))

;;;###autoload
(defun zenit/decrease-font-size (count &optional increment)
  "Shrinks the font size across the current and child frames."
  (interactive "p")
  (zenit-adjust-font-size (* (- count) (or increment zenit-font-increment))))

;;;###autoload
(defun zenit/reset-font-size ()
  "Reset font size and `text-scale'.

Assuming it has been adjusted via `zenit/increase-font-size' and
`zenit/decrease-font-size', or `text-scale-*' commands."
  (interactive)
  (let (success)
    (when (and (boundp 'text-scale-mode-amount)
               (/= text-scale-mode-amount 0))
      (text-scale-set 0)
      (setq success t))
    (cond ((bound-and-true-p zenit-big-font-mode)
           (message "Disabling `zenit-big-font-mode'")
           (zenit-big-font-mode -1)
           (setq success t))
          ((zenit-adjust-font-size nil)
           (setq success t)))
    (unless success
      (user-error "The font hasn't been resized"))))

;;;###autoload
(define-minor-mode zenit-big-font-mode
  "A global mode that resizes the font, for streams,
screen-sharing and presentations. Uses `zenit-big-font' if its
set, otherwise uses `zenit-font' (falling back to your system
font). Also resizees `zenit-variable-pitch-font' and
`zenit-serif-font'."
  :init-value nil
  :lighter " BIG"
  :global t
  :group 'zenit
  (if zenit-big-font
      ;; Use `zenit-big-font' in lieu of `zenit-font'
      (zenit-adjust-font-size
       (when zenit-big-font-mode
         (font-get (zenit-normalize-font zenit-big-font) :size))
       t `((zenit-font . ,zenit-big-font)))
    ;; Resize the current font
    (zenit-adjust-font-size nil)
    (when zenit-big-font-mode
      (zenit-adjust-font-size zenit-big-font-increment))))

(provide 'zenit-lib '(fonts))
