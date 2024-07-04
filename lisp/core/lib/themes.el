;; lisp/core/lib/themes.el -*- lexical-binding: t; -*-

(defun zenit--custom-theme-set-face (spec)
  (cond ((listp (car spec))
         (cl-loop for face in (car spec)
                  collect
                  (car (zenit--custom-theme-set-face (cons face (cdr spec))))))
        ((keywordp (cadr spec))
         `((,(car spec) ((t ,(cdr spec))))))
        (`((,(car spec) ,(cdr spec))))))

;;;###autoload
(defconst zenit-customize-theme-hook nil)

(add-hook! 'zenit-load-theme-hook
  (defun zenit-apply-customized-faces-h ()
    "TODO"
    (run-hooks 'zenit-customize-theme-hook)))

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
         (let (custom--inhibit-theme-enable)
           (dolist (theme (ensure-list (or ,theme 'user)))
             (when (or (eq theme 'user)
                       (custom-theme-enabled-p theme))
               (apply #'custom-theme-set-faces theme
                      (mapcan #'zenit--custom-theme-set-face
                              (list ,@specs)))))))
       (unless zenit-theme (funcall #',fn))
       (add-hook 'zenit-customize-theme-hook #',fn 'append))))

;;;###autoload
(defmacro custom-set-faces! (&rest specs)
  "Apply a list of face SPECS as user customizations.
This is a convenience macro alternative to `custom-set-face'
which allows for a simplified face format, and takes care of load
order issues, so you can use zenit-themes' API without worry."
  (declare (indent defun))
  `(custom-theme-set-faces! 'user ,@specs))

;;;###autoload
(defun zenit/reload-theme ()
  "Reload the current color theme."
  (interactive)
  (let ((theme (or (car-safe custom-enabled-themes) zenit-theme)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes))
    (load-theme zenit-theme 'noconfirm)
    (zenit/reload-font)))
