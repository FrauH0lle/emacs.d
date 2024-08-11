;; ui/tabs/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

;; PATCH 2024-08-02: Slightly adjust the formatted output of
;;   `tab-bar-tab-name-format-default'.
(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'zenit-el-patch)
  (require 'tab-bar))

;; PATCH 2024-08-02: `tab-bar'
(el-patch-feature tab-bar)

(after! tab-bar
  (el-patch-defun tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format (el-patch-swap "%d " "  #%d: ") i) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab)))))

(use-package! tab-bar
  :defer t
  :config
  (setq! tab-bar-history-limit 25
         tab-bar-show 1
         tab-bar-tab-hints t)

  (protect-macros!
    (custom-set-faces!
      ;; The tab bar's appearance
      `(tab-bar
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 6 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Inactive tabs
      `(tab-bar-tab-inactive
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Active tab
      `(tab-bar-tab
        :background ,(face-attribute 'default :background)
        :foreground ,(face-attribute 'font-lock-keyword-face :foreground nil t)
        :box
        (:line-width 3 :color ,(face-attribute 'default :background) :style nil))

      ;; The tab bar's appearance
      `(tab-bar-tab-ungrouped
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))

      ;; Inactive tabs
      `(tab-bar-tab-group-inactive
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))

      ;; Active tab
      `(tab-bar-tab-group-current
        :background ,(face-attribute 'mode-line-inactive :background) :foreground ,(face-attribute 'default :foreground)
        :box (:line-width 3
              :color ,(face-attribute 'mode-line-inactive :background)
              :style nil)))))
