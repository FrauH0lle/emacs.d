;; ui/tabs/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

;; PATCH 2024-08-02: Slightly adjust the formatted output of
;;   `tab-bar-tab-name-format-default'.
(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'tab-bar))

;; PATCH 2025-04-23: `tab-bar'
(el-patch-feature tab-bar)

(after! tab-bar
  (el-patch-defun tab-bar-tab-name-format-hints (name _tab i)
    "Show absolute numbers on tabs in the tab bar before the tab name.
It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints (concat (format (el-patch-swap "%d " "  #%d: ") i) name) name)))


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


;; Based on
;; https://andreyor.st/posts/2020-05-10-making-emacs-tabs-look-like-in-atom/
(defvar +tab-line-tab-min-width 10
  "Minimum width of a tab in characters.")

(defvar +tab-line-tab-max-width 30
  "Maximum width of a tab in characters.")

(use-package! tab-line
  :defer t
  :init
  (protect-macros!
    (custom-set-faces!
      ;; The tab-line bar's appearance
      `(tab-line
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Inactive tabs
      `(tab-line-tab-inactive
        :background ,(face-attribute 'mode-line-inactive :background)
        :foreground ,(face-attribute 'mode-line-inactive :foreground)
        :box
        (:line-width 3 :color ,(face-attribute 'mode-line-inactive :background) :style nil))
      ;; Active tab
      `(tab-line-tab-current
        :background ,(face-attribute 'default :background)
        :foreground ,(face-attribute 'font-lock-keyword-face :foreground nil t)
        :box
        (:line-width 3 :color ,(face-attribute 'default :background) :style nil))))
  :config
  (setq! tab-line-tab-name-function #'+tab-line-tab-name-fn
         tab-line-close-button-show nil
         tab-line-new-button-show nil)

  ;; HACK 2024-12-05: Recalculate tab width on frame or window resize events.
  (add-hook! 'window-configuration-change-hook
    (defun +tabs-resest-tab-line-cache-h ()
      "Reset `tab-line' cache in all windows."
      (dolist (window (window-list))
        (set-window-parameter window 'tab-line-cache nil)))))
