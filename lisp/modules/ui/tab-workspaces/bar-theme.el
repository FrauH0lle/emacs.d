;; ui/tab-workspaces/bar-theme.el -*- lexical-binding: t; -*-


(let* ((color-fallback-light (face-attribute 'default :foreground))
       (fallback-color-dark (face-attribute 'default :background))
       (bg-default (or (face-attribute 'default :background)
                       color-fallback-light))
       (fg-default (or (face-attribute 'default :foreground)
                       fallback-color-dark))
       (bg-modeline-inactive (or (face-attribute
                                  'mode-line-inactive :background)
                                 fallback-color-dark))
       (fg-modeline-inactive (or (face-attribute
                                  'mode-line-inactive :foreground)
                                 color-fallback-light))
       (bg-tab-inactive bg-modeline-inactive)
       (fg-tab-inactive fg-modeline-inactive)
       (fg-tab-active fg-default)
       (bg-tab-active bg-default))
  (custom-set-faces
   ;; The tab bar's appearance
   `(tab-bar
     ((t (:background ,bg-tab-inactive
          :foreground ,fg-tab-inactive
          :box
          (:line-width 3 :color ,bg-tab-inactive :style nil)))))
   ;; Inactive tabs
   `(tab-bar-tab-inactive
     ((t (:background ,bg-tab-inactive
          :foreground ,fg-tab-inactive
          :box
          (:line-width 3 :color ,bg-tab-inactive :style nil)))))
   ;; Active tab
   `(tab-bar-tab
     ((t (:background ,bg-tab-active :foreground ,fg-tab-active
          :box
          (:line-width 3 :color ,bg-tab-active :style nil)))))

   ;; The tab bar's appearance
   `(tab-bar-tab-ungrouped
     ((t (:background ,bg-tab-inactive
          :foreground ,fg-tab-inactive
          :box
          (:line-width 3 :color ,bg-tab-inactive :style nil)))))

   ;; Inactive tabs
   `(tab-bar-tab-group-inactive
     ((t (:background ,bg-tab-inactive
          :foreground ,fg-tab-inactive
          :box
          (:line-width 3 :color ,bg-tab-inactive :style nil)))))

   ;; Active tab
   `(tab-bar-tab-group-current
     ((t (:background ,bg-tab-inactive :foreground ,fg-tab-active
          :box (:line-width 3
                :color ,bg-tab-inactive
                :style nil)))))))
