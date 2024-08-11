;; config/default/+evil.el -*- lexical-binding: t; -*-
;; (benchmark-run 1 (progn
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

(eval-when-compile
  (message "before modulep! +bindings; cl--not-toplevel is: %s" cl--not-toplevel))

(eval-when! (modulep! +bindings)
  ;; (include! "+evil-bindings")
  ;; (include! "+evil-bindings")
  ;; (defvar cl--not-toplevel nil)
  ;; (eval-when-compile
  ;;   (message "after modulep! +bindings; cl--not-toplevel is: %s" cl--not-toplevel))
  ;; (setq cl--not-toplevel nil)
  ;; (eval-when-compile
  ;;   (message "in let binding; cl--not-toplevel is: %s" cl--not-toplevel))

  ;; (when (macroexp-compiling-p)
  ;;   )
  ;; (eval-when-compile
  ;;   (byte-compile-file
  ;;    (file-name-concat
  ;;     (dir!)
  ;;     (file-name-with-extension "+evil-bindings" ".el"))))

  (compile-along! "+evil-bindings")
  ;; (require 'async-bytecomp)
    ;; (byte-compile-file
    ;;  (file-name-concat
    ;;   (dir!)
    ;;   (file-name-with-extension "+evil-bindings" ".el")))
  ;; (cl-eval-when
  ;;     (compile)
  ;;   (require 'async-bytecomp)
  ;;   (async-byte-compile-file
  ;;    (file-name-concat
  ;;     (dir!)
  ;;     (file-name-with-extension "+evil-bindings" ".el"))))
  (load! "+evil-bindings")
  )
;; ))
