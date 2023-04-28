;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-ui.el

(describe "core/lib/ui"

  (load! "lib/ui" zenit-core-dir)

  (describe "zenit-resize-window"
    (it "resizes selected window"
      (spy-on 'enlarge-window)
      (zenit-resize-window nil 5)
      (expect 'enlarge-window :to-have-been-called)))


  (describe "zenit-quit-p"
    (it "returns t if no real buffers are open"
      (spy-on 'zenit-real-buffer-list :and-return-value nil)
      (expect (zenit-quit-p) :to-be t))

    (it "prompts if real buffers are open"
      (spy-on 'zenit-real-buffer-list :and-return-value t)
      (spy-on 'yes-or-no-p :and-return-value t)
      (expect (zenit-quit-p) :to-be t)
      (expect 'yes-or-no-p :to-have-been-called-with (format "››› %s" "Quit Emacs?")))

    (it "else aborts"
      (spy-on 'zenit-real-buffer-list :and-return-value t)
      (spy-on 'yes-or-no-p :and-return-value nil)
      (spy-on 'message)
      (expect (zenit-quit-p) :to-be nil)
      (expect 'message :to-have-been-called-with "Aborted")))


  (describe "zenit-recenter-a"
    (it "calls recenter"
      (spy-on 'recenter)
      (zenit-recenter-a)
      (expect 'recenter :to-have-been-called)))


  (describe "zenit-shut-up-a"
    (it "silences message"
      (expect (zenit-shut-up-a 'message '("hi")) :to-be nil)))


  (describe "zenit-disable-show-paren-mode-h"
    (it "disables show-paren-mode"
      (with-temp-buffer
        (zenit-disable-show-paren-mode-h)
        (expect show-paren-mode :to-be nil))))


  (describe "zenit/toggle-line-numbers"
    (before-each
      (setq display-line-numbers-type t))

    (it "toggles line numbers style"
      (spy-on 'message)
      (with-temp-buffer
        (conf-mode)
        (zenit/toggle-line-numbers)
        (expect 'message :to-have-been-called-with "Switched to %s line numbers" "relative")
        (zenit/toggle-line-numbers)
        (expect 'message :to-have-been-called-with "Switched to %s line numbers" "disabled")
        (zenit/toggle-line-numbers)
        (expect 'message :to-have-been-called-with "Switched to %s line numbers" "normal"))))


  (describe "zenit/delete-frame-with-prompt"
    (it "prompts if frames are open"
      (spy-on 'frame-list :and-return-value '(t t t))
      (spy-on 'zenit-quit-p :and-return-value t)
      (spy-on 'delete-frame)
      (zenit/delete-frame-with-prompt)
      (expect 'zenit-quit-p :to-have-been-called)
      (expect 'delete-frame :to-have-been-called))

    (it "else quits emacs"
      (spy-on 'frame-list)
      (spy-on 'save-buffers-kill-emacs)
      (zenit/delete-frame-with-prompt)
      (expect 'save-buffers-kill-emacs :to-have-been-called)))


  (describe "zenit--enlargened-forget-last-wconf-h"
    (it "calls set-frame-parameter"
      (spy-on 'set-frame-parameter)
      (spy-on 'remove-hook)
      (zenit--enlargened-forget-last-wconf-h)
      (expect 'set-frame-parameter :to-have-been-called-with nil 'zenit--maximize-last-wconf nil)
      (expect 'set-frame-parameter :to-have-been-called-with nil 'zenit--enlargen-last-wconf nil)
      (expect 'remove-hook :to-have-been-called-with 'zenit-switch-window-hook #'zenit--enlargened-forget-last-wconf-h)))


  (describe "zenit/window-maximize-buffer"
    (before-each
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (dotimes (i 3)
          (split-window-right))))

    (after-each
      (delete-other-windows))

    (it "maximizes the selected window"
      (zenit/window-maximize-buffer)
      (expect (length (window-list)) :to-be 1))

    (it "restores the previous window configuration on second activation"
      (let ((initial-window-count (length (window-list))))
        (zenit/window-maximize-buffer)
        (expect (length (window-list)) :to-be 1)
        (zenit/window-maximize-buffer)
        (expect (length (window-list)) :to-be initial-window-count)))

    (it "does not restore the previous window configuration when called with a prefix argument"
      (zenit/window-maximize-buffer)
      (expect (length (window-list)) :to-be 1)
      (zenit/window-maximize-buffer t)
      (expect (length (window-list)) :to-be 1)))


  (describe "zenit/window-enlargen"
    (before-each
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (dotimes (i 3)
          (split-window-right))))

    (it "enlargens the selected window without closing other windows"
      (let ((initial-window-count (length (window-list))))
        (zenit/window-enlargen)
        (expect (length (window-list)) :to-be initial-window-count))))


  (describe "zenit/window-maximize-horizontally"
    (before-each
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (dotimes (i 3)
          (split-window-right))))

    (it "deletes all windows to the left and right of the current window"
      (let ((initial-window-count (length (window-list))))
        (zenit/window-maximize-horizontally)
        (expect (length (window-list)) :to-be 1)))

    (it "leaves the selected window intact"
      (let ((initial-selected-window (selected-window)))
        (zenit/window-maximize-horizontally)
        (expect (selected-window) :to-be initial-selected-window))))


  (describe "zenit/window-maximize-vertically"
    (before-each
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (dotimes (i 3)
          (split-window-below))))

    (it "deletes all windows above and below the current window"
      (let ((initial-window-count (length (window-list))))
        (zenit/window-maximize-vertically)
        (expect (length (window-list)) :to-be 1)))

    (it "leaves the selected window intact"
      (let ((initial-selected-window (selected-window)))
        (zenit/window-maximize-vertically)
        (expect (selected-window) :to-be initial-selected-window))))


  (describe "zenit/set-frame-opacity"
    (it "sets the frame opacity"
      (zenit/set-frame-opacity 75)
      (expect (frame-parameter nil 'alpha) :to-be 75)))


  (describe "zenit/narrow-buffer-indirectly"
    :var (original-buffer indirect-buffer test-string)

    (before-each
      (setq test-string "Hello, World!")
      (setq original-buffer (generate-new-buffer "original-buffer"))
      (with-current-buffer original-buffer
        (insert test-string)))

    (after-each
      (kill-buffer original-buffer)
      (when (buffer-live-p indirect-buffer)
        (kill-buffer indirect-buffer)))

    (it "creates an indirect buffer"
      (setq indirect-buffer (zenit/narrow-buffer-indirectly (point-min) (point-max)))
      (expect (buffer-live-p indirect-buffer) :to-be t))

    (it "narrow to the specified region"
      (with-current-buffer original-buffer
        (goto-char (point-min))
        (forward-word)
        (let ((narrow-start (point))
              (narrow-end (point-max)))
          (setq indirect-buffer (zenit/narrow-buffer-indirectly narrow-start narrow-end))
          (with-current-buffer indirect-buffer
            (let ((indirect-narrow-start (1+ (- narrow-start (point-min))))
                  (indirect-narrow-end (1+ (- narrow-end (point-min)))))
              (narrow-to-region indirect-narrow-start indirect-narrow-end)
              (expect (buffer-string) :to-equal (buffer-substring narrow-start narrow-end))))))))


  (describe "zenit/widen-indirectly-narrowed-buffer"
    :var (original-buffer indirect-buffer test-string)

    (before-each
      (setq test-string "Hello, World!")
      (setq original-buffer (generate-new-buffer "original-buffer"))
      (with-current-buffer original-buffer
        (insert test-string)))

    (after-each
      (kill-buffer original-buffer)
      (when (buffer-live-p indirect-buffer)
        (kill-buffer indirect-buffer)))

    (it "widens an indirectly narrowed buffer"
      (with-current-buffer original-buffer
        (goto-char (point-min))
        (forward-word)
        (let ((narrow-start (point))
              (narrow-end (point-max)))
          (setq indirect-buffer (zenit/narrow-buffer-indirectly narrow-start narrow-end))
          (with-current-buffer indirect-buffer
            (let ((indirect-narrow-start (1+ (- narrow-start (point-min))))
                  (indirect-narrow-end (1+ (- narrow-end (point-min)))))
              (narrow-to-region indirect-narrow-start indirect-narrow-end)
              (expect (buffer-string) :to-equal (buffer-substring narrow-start narrow-end)))
            (zenit/widen-indirectly-narrowed-buffer)
            (expect (buffer-string) :to-equal test-string))))))


  (describe "zenit/toggle-narrow-buffer"
    (it "narrows a widened buffer"
      (spy-on 'buffer-narrowed-p)
      (spy-on 'region-active-p :and-return-value t)
      (spy-on 'narrow-to-region)
      (zenit/toggle-narrow-buffer (point-min) (point-max))
      (expect 'narrow-to-region :to-have-been-called))

    (it "widens a narrowed buffer"
      (spy-on 'buffer-narrowed-p :and-return-value t)
      (spy-on 'widen)
      (zenit/toggle-narrow-buffer (point-min) (point-max))
      (expect 'widen :to-have-been-called))))
