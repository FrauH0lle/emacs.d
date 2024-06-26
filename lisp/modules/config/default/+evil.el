;; config/default/+evil.el -*- lexical-binding: t; -*-

(defun +default-disable-delete-selection-mode-h ()
  (delete-selection-mode -1))
(add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
(add-hook 'evil-insert-state-exit-hook  #'+default-disable-delete-selection-mode-h)


;;
;;; Keybindings

;; This section is dedicated to "fixing" certain keys so that they behave
;; sensibly (and consistently with similar contexts).

;; Make SPC u SPC u [...] possible (#747)
(map! :map universal-argument-map
      :prefix zenit-leader-key     "u" #'universal-argument-more
      :prefix zenit-leader-alt-key "u" #'universal-argument-more)

(eval-when! (modulep! +bindings)
  (include! "+evil-bindings"))
