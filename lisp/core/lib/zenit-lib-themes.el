;; lisp/core/lib/zenit-lib-themes.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

;; `zenit-lib-fonts'
(declare-function zenit/reload-font "zenit-lib-fonts" ())


;;;###autoload
(defconst zenit-customize-theme-hook nil
  "Hook run after a theme or face has been enabled or customized.")

;;;###autoload
(defun zenit--run-customize-theme-hook (fn)
  "Run FN, but suppress any writes to `custom-file'."
  (letf! (defun put (symbol prop value)
           (unless (string-prefix-p "saved-" (symbol-name prop))
             (funcall put symbol prop value)))
    (let (custom--inhibit-theme-enable)
      (funcall fn))))

(add-hook! 'zenit-load-theme-hook
  (defun zenit-apply-customized-faces-h ()
    "Run `zenit-customize-theme-hook'."
    (run-hook-wrapped 'zenit-customize-theme-hook #'zenit--run-customize-theme-hook)))

(defun zenit--normalize-face-spec (spec)
  "Normalize face SPEC into a suitable format.
Converts shorthand face specifications into the standard format
expected by `custom-theme-set-faces'. Handles both single faces
and lists of faces, converting property lists into proper face
attribute specifications."
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (zenit--normalize-face-spec (cons face (cdr spec))))))
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defmacro custom-theme-set-faces! (theme &rest specs)
  "Apply a list of face SPECS as user customizations for THEME.
THEME can be a single symbol or list thereof. If nil, apply these
settings to all themes. It will apply to all themes once they are
loaded."
  (declare (indent defun))
  (let ((fn (gensym "zenit--customize-themes-h-")))
    `(progn
       (defun ,fn ()
         (dolist (theme (ensure-list (or ,theme 'user)))
           (if (or (eq theme 'user)
                   (custom-theme-enabled-p theme))
               (apply #'custom-theme-set-faces theme
                      (mapcan #'zenit--normalize-face-spec
                              (list ,@specs))))))
       ;; Apply the changes immediately if the user is using the default theme
       ;; or the theme has already loaded. This allows you to evaluate these
       ;; macros on the fly and customize your faces iteratively.
       (when (or (get 'zenit-theme 'previous-themes)
                 (null zenit-theme))
         (zenit--run-customize-theme-hook #',fn))
       ;; FIXME Prevent clobbering this on-the-fly
       (add-hook 'zenit-customize-theme-hook #',fn 100))))

;;;###autoload
(defmacro custom-set-faces! (&rest specs)
  "Apply a list of face SPECS as user customizations.
This is a convenience macro alternative to `custom-set-face'
which allows for a simplified face format, and takes care of load
order issues, so you can use zenit-themes' API without worry."
  (declare (indent defun))
  `(custom-theme-set-faces! 'user ,@specs))

(defvar zenit-theme)
(defvar zenit-load-theme-hook)
;;;###autoload
(defun zenit/reload-theme ()
  "Reload the current Emacs theme."
  (interactive)
  (unless zenit-theme
    (user-error "No theme is active"))
  (let ((themes (copy-sequence custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (let (zenit-load-theme-hook)
      (mapc #'enable-theme (reverse themes)))
    (zenit-run-hooks 'zenit-load-theme-hook)
    (zenit/reload-font)
    (message "%s %s"
             (propertize
              (format "Reloaded %d theme%s:"
                      (length themes)
                      (if (cdr themes) "s" ""))
              'face 'bold)
             (mapconcat #'prin1-to-string themes ", "))))

(provide 'zenit-lib '(themes))
