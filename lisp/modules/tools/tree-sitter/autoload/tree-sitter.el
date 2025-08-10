;; tools/tree-sitter/autoload/tree-sitter.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +tree-sitter--major-mode-remaps-alist nil)

;;;###autodef (fset 'tree-sitter! #'ignore)
(defun tree-sitter! ()
  "Dispatch to turn on tree sitter.

Used as a hook function which turns on highlighting provided by
`treesit-langs' selectively according to `+tree-sitter-hl-enabled-modes'"
  ;; Conditionally enable `tree-sitter-hl-mode'
  (let ((mode (bound-and-true-p tree-sitter-hl-mode)))
    (when-let* ((mode (if (pcase +tree-sitter-hl-enabled-modes
                            (`(not . ,modes) (not (memq major-mode modes)))
                            ((and `(,_ . ,_) modes) (memq major-mode modes))
                            (bool bool))
                          (unless mode t)
                        (if mode nil))))
      (treesit-hl-toggle mode))))

;;;###autodef (fset 'set-tree-sitter! #'ignore)
(defun set-tree-sitter! (mode ts-mode &optional recipes)
  "Remap major MODE to TS-MODE.

MODE and TS-MODE are major mode symbols. If RECIPES is provided, fall back to
MODE if RECIPES don't pass `treesit-ready-p' when activating TS-MODE. Use this
for ts modes that error out instead of failing gracefully.

RECIPES are an alist of plists with the format (LANG &key URL REV SOURCE-DIR CC
CPP COMMIT), which will be transformed into entries for
`treesit-language-source-alist' (which descrie what each of these keys mean).
Note that COMMIT is only available in Emacs >=31."
  (declare (indent 2))
  (cl-check-type mode symbol)
  (cl-check-type ts-mode symbol)
  (setq recipes (ensure-list recipes))
  (dolist (m (ensure-list mode))
    (add-to-list
     '+tree-sitter--major-mode-remaps-alist
     (cons
      m (let (ensured?)
          (lambda ()
            (funcall
             ;; Because standard major-mode remapping doesn't offer graceful
             ;; failure in some cases, I implement it myself:
             (cond ((null recipes) m)
                   ((not (fboundp ts-mode))
                    (message "Couldn't find %S, using %S instead" ts-mode m)
                    m)
                   ((and (fboundp 'treesit-available-p)
                         (treesit-available-p)
                         (fboundp ts-mode)
                         ;; Only prompt once, and log other times.
                         (cl-every (if ensured?
                                       (zenit-rpartial #'treesit-ready-p 'message)
                                     #'treesit-ensure-installed)
                                   (cl-loop for r in recipes
                                            if (listp r)
                                            collect (car r)
                                            else collect (list r))))
                    ts-mode)
                   ((setq ensured? t)
                    m))))))))
  (with-eval-after-load 'treesit
    (dolist (recipe recipes)
      (cl-destructuring-bind (name &key url rev source-dir cc cpp commit)
          (ensure-list recipe)
        (setf (alist-get name treesit-language-source-alist)
              (append (list url rev source-dir cc cpp)
                      ;; COMPAT: 31.1 introduced a COMMIT recipe argument. On
                      ;;   <=30.x, extra arguments will trigger an arity error
                      ;;   when installing grammars.
                      (if (eq (cdr (func-arity 'treesit--install-language-grammar-1))
                              'many)
                          (list commit))))))))

;;;###autoload
(defun +tree-sitter-get-textobj (group &optional query)
  "A wrapper around `evil-textobj-tree-sitter-get-textobj' to
prevent eager expansion."
  (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

;;;###autoload
(defun +tree-sitter-goto-textobj (group &optional previous end query)
  "Thin wrapper that returns the symbol of a named function, used in
keybindings."
  (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
    (fset sym (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj group previous end query)))
    sym))
