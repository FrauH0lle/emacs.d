;; ui/workspaces/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'persp-mode))

;; PATCH Ensure current-buffer is up to date when a perspective is activated.
;;   See https://github.com/Bad-ptr/persp-mode.el/pull/149. The original
;;   behavior should be considered a bug but `persp-mode' is not so actively
;;   maintained anymore.
(el-patch-cl-defun persp-activate
    (persp &optional (frame-or-window (selected-frame)) new-frame-p)
  (when frame-or-window
    (let (old-persp type)
      (cl-typecase frame-or-window
        (frame
         (setq old-persp (get-frame-persp frame-or-window)
               type 'frame))
        (window
         (setq old-persp (get-window-persp frame-or-window)
               type 'window)))
      (when  (or new-frame-p
                 (not (eq old-persp persp)))
        (unless new-frame-p
          (persp--deactivate frame-or-window persp))
        (cl-case type
          (frame
           (setq persp-last-persp-name (safe-persp-name persp))
           (set-frame-persp persp frame-or-window)
           (when persp-init-frame-behaviour
             (persp-restore-window-conf frame-or-window persp new-frame-p))
           (run-hook-with-args 'persp-activated-functions 'frame frame-or-window persp))
          (window
           (set-window-persp persp frame-or-window)
           (let ((cbuf (window-buffer frame-or-window)))
             (unless (persp-contain-buffer-p cbuf persp)
               (persp-set-another-buffer-for-window cbuf frame-or-window persp)))
           (run-hook-with-args 'persp-activated-functions 'window frame-or-window persp))))))
  (el-patch-add (set-buffer (window-buffer (selected-window)))))
