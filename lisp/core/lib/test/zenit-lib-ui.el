;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-ui.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'ui)
(require 'zenit-lib)

(zenit-deftest zenit-resize-window
  (:doc "`zenit-resize-window' is defined")
  (should (fboundp 'zenit-resize-window)))

(zenit-deftest zenit-quit-p ()
  ,test
  (test)
  :doc "`zenit-quit-p' returns t if no real buffers are open"
  (letf! ((defun zenit-real-buffer-list () nil))
    (should (zenit-quit-p)))
  :doc "`zenit-quit-p' prompts if real buffers are open"
  (letf! ((defun zenit-real-buffer-list () t)
          (defun yes-or-no-p (&rest _) t))
    (should (zenit-quit-p)))
  :doc "`zenit-quit-p' else aborts"
  (letf! ((defun zenit-real-buffer-list () t)
          (defun yes-or-no-p (&rest _) nil))
    (should-not (quiet!! (zenit-quit-p)))))

(zenit-deftest zenit-recenter-a
  (:doc "`zenit-recenter-a' is defined")
  (should (fboundp 'zenit-recenter-a)))

(zenit-deftest zenit-shut-up-a
  (:doc "`zenit-shut-up-a' is defined")
  (should (fboundp 'zenit-shut-up-a)))

(zenit-deftest zenit-disable-show-paren-mode-h
  (:doc "`zenit-disable-show-paren-mode-h' is defined")
  (should (fboundp 'zenit-disable-show-paren-mode-h)))

(zenit-deftest zenit/toggle-line-numbers
  (:doc "`zenit/toggle-line-numbers' is defined")
  (should (fboundp 'zenit/toggle-line-numbers)))

(zenit-deftest zenit/delete-frame-with-prompt
  (:doc "`zenit/delete-frame-with-prompt' is defined")
  (should (fboundp 'zenit/delete-frame-with-prompt)))

(zenit-deftest zenit/window-maximize-buffer
  (:doc "`zenit/window-maximize-buffer' is defined")
  (should (fboundp 'zenit/window-maximize-buffer)))

(zenit-deftest zenit/window-enlargen
  (:doc "`zenit/window-enlargen' is defined")
  (should (fboundp 'zenit/window-enlargen)))

(zenit-deftest zenit/window-maximize-horizontally
  (:doc "`zenit/window-maximize-horizontally' is defined")
  (should (fboundp 'zenit/window-maximize-horizontally)))

(zenit-deftest zenit/window-maximize-vertically
  (:doc "`zenit/window-maximize-vertically' is defined")
  (should (fboundp 'zenit/window-maximize-vertically)))

(zenit-deftest zenit/set-frame-opacity
  (:doc "`zenit/set-frame-opacity' is defined")
  (should (fboundp 'zenit/set-frame-opacity)))

(zenit-deftest zenit--narrowed-base-buffer
  (:doc "`zenit--narrowed-base-buffer' is defined")
  (should (boundp 'zenit--narrowed-base-buffer)))

(zenit-deftest zenit/narrow-buffer-indirectly
  (:doc "`zenit/narrow-buffer-indirectly' is defined")
  (should (fboundp 'zenit/narrow-buffer-indirectly)))

(zenit-deftest zenit/widen-indirectly-narrowed-buffer
  (:doc "`zenit/widen-indirectly-narrowed-buffer' is defined")
  (should (fboundp 'zenit/widen-indirectly-narrowed-buffer)))

(zenit-deftest zenit/toggle-narrow-buffer
  (:doc "`zenit/toggle-narrow-buffer' is defined")
  (should (fboundp 'zenit/toggle-narrow-buffer)))
