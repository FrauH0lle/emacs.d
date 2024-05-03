;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  "Move the current list of candidates to your choice of minibuffer
completion UI."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (cond ((and (modulep! :completion vertico)
                   (fboundp #'consult-completion-in-region))
              (consult-completion-in-region beg end table pred))
             (t (error "No minibuffer completion UI available for moving to!")))))))

;;;###autoload
(defun +corfu-smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t (call-interactively #'corfu-insert-separator))))
