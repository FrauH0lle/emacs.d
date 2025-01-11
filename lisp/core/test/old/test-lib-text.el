;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-text.el


(describe "core/lib/text"

  (load! "lib/text" zenit-core-dir)

    (before-each
      (setq test-dir (make-temp-file "zenit-test-dir" t))
      (find-file (expand-file-name "test-buffer.el" test-dir)))

    (after-each
      (kill-buffer (current-buffer))
      (delete-directory test-dir t))


  (describe "zenit-surrounded-p"
    (it "returns t when point is surrounded by braces"
      (with-temp-buffer
        (insert "foo {bar} baz")
        (goto-char 7)
        (expect (zenit-surrounded-p '(:beg 5 :end 8 :op "{" :cl "}")) :to-be t)))

    (it "returns nil when point is not surrounded by braces"
      (with-temp-buffer
        (insert "foo {bar} baz")
        (goto-char 4)
        (expect (zenit-surrounded-p '(:beg 5 :end 8 :op "{" :cl "}")) :to-be nil)))

    (it "returns t when point is surrounded by braces on the same line and whitespace is balanced"
      (with-temp-buffer
        (insert "foo { bar } baz")
        (goto-char 8)
        (expect (zenit-surrounded-p '(:beg 5 :end 11 :op "{" :cl "}") t t) :to-be t)))

    (it "returns nil when point is surrounded by braces but whitespace is not balanced"
      (with-temp-buffer
        (insert "foo {  bar } baz")
        (goto-char 9)
        (expect (zenit-surrounded-p '(:beg 5 :end 12 :op "{" :cl "}") t t) :to-be nil)))

    (it "returns t when point is surrounded by braces on adjacent lines"
      (with-temp-buffer
        (insert "foo {\n bar\n} baz")
        (goto-char 8)
        (expect (zenit-surrounded-p '(:beg 5 :end 12 :op "{" :cl "}")) :to-be t)))

    (it "returns t when point is surrounded by parentheses"
      (with-temp-buffer
        (insert "foo (bar) baz")
        (goto-char 7)
        (expect (zenit-surrounded-p '(:beg 5 :end 8 :op "(" :cl ")")) :to-be t))))


(describe "zenit-point-in-comment-p"
  (it "returns t when point is within a single-line comment"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo ;; this is a comment\nbar")
      (goto-char 10)
      (expect (zenit-point-in-comment-p) :to-be t)))

  (it "returns nil when point is not within a single-line comment"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo ;; this is a comment\nbar")
      (goto-char 5)
      (expect (zenit-point-in-comment-p) :to-be nil)))

  (it "returns t when point is within a multi-line comment"
    (with-temp-buffer
      (c-mode)
      (insert "foo\n/* this is a\n multi-line comment */\nbar")
      (goto-char 15)
      (expect (zenit-point-in-comment-p) :to-be t)))

  (it "returns nil when point is not within a multi-line comment"
    (with-temp-buffer
      (c-mode)
      (insert "foo\n/* this is a\n multi-line comment */\nbar")
      (goto-char 5)
      (expect (zenit-point-in-comment-p) :to-be nil))))


(describe "zenit-point-in-string-p"
  (it "returns t when point is within a double-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo \"this is a string\" bar")
      (goto-char 10)
      (expect (zenit-point-in-string-p) :to-be-truthy)))

  (it "returns nil when point is not within a double-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo \"this is a string\" bar")
      (goto-char 5)
      (expect (zenit-point-in-string-p) :to-be nil)))

  (it "returns t when point is within a single-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo 'this is a string' bar")
      (goto-char 10)
      (expect (zenit-point-in-string-p) :to-be-truthy)))

  (it "returns nil when point is not within a single-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo 'this is a string' bar")
      (goto-char 5)
      (expect (zenit-point-in-string-p) :to-be nil))))


(describe "zenit-point-in-string-or-comment-p"
  (it "returns t when point is within a double-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo \"this is a string\" bar")
      (goto-char 10)
      (expect (zenit-point-in-string-or-comment-p) :to-be-truthy)))

  (it "returns t when point is within a single-quoted string"
    (with-temp-buffer
      (conf-mode)
      (insert "foo 'this is a string' bar")
      (goto-char 10)
      (expect (zenit-point-in-string-or-comment-p) :to-be-truthy)))

  (it "returns t when point is within a comment"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo ; this is a comment\nbar")
      (goto-char 10)
      (expect (zenit-point-in-string-or-comment-p) :to-be-truthy)))

  (it "returns nil when point is not within a string or comment"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "foo \"this is a string\" bar")
      (goto-char 5)
      (expect (zenit-point-in-string-or-comment-p) :to-be nil))))


(describe "zenit-region-active-p"
  (it "returns t when the region is active"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 1)
      (set-mark 5)
      (activate-mark)
      (expect (zenit-region-active-p) :to-be t)))

  (it "returns nil when the region is inactive"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 1)
      (set-mark 5)
      (deactivate-mark)
      (expect (zenit-region-active-p) :to-be nil))))


(describe "zenit-region-beginning"
  (it "returns the beginning of the active region"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 1)
      (set-mark 5)
      (activate-mark)
      (goto-char 9)
      (expect (zenit-region-beginning) :to-equal 5))))


(describe "zenit-region-end"
  (it "returns the end of the active region"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 1)
      (set-mark 5)
      (activate-mark)
      (goto-char 9)
      (expect (zenit-region-end) :to-equal 9))))


(describe "zenit-thing-at-point-or-region"
  (it "returns the text in the active region"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 1)
      (set-mark 6)
      (activate-mark)
      (goto-char 9)
      (expect (zenit-thing-at-point-or-region 'symbol) :to-equal "is ")))

  (it "returns the thing at point when the region is inactive"
    (with-temp-buffer
      (insert "This is a test")
      (goto-char 6)
      (deactivate-mark)
      (expect (zenit-thing-at-point-or-region 'symbol) :to-equal "is"))))


(describe "zenit--bol-bot-eot-eol"
  (it "returns beginning of line, beginning of top line, end of top line, and end of line correctly"
    (with-temp-buffer
      (insert "Line 1\nLine 2\nLine 3")
      (goto-char 10)
      (let ((result (zenit--bol-bot-eot-eol)))
        ;; beginning of line
        (expect (nth 0 result) :to-equal 8)
           ;; beginning of top line
        (expect (nth 1 result) :to-equal 8)
           ;; end of top line
        (expect (nth 2 result) :to-equal 14)
         ;; end of line
        (expect (nth 3 result) :to-equal 14)))))


(describe "zenit/backward-to-bol-or-indent"
  (it "moves point to the first non-whitespace character on the line"
    (with-temp-buffer
      (insert "  Line 1\n  Line 2\n  Line 3")
      (goto-char 12)
      (zenit/backward-to-bol-or-indent)
      (expect (point) :to-equal 10)))

  (it "moves point to the beginning of the line if it's already at the first non-whitespace character"
    (with-temp-buffer
      (insert "  Line 1\n  Line 2\n  Line 3")
      (goto-char 12)
      (zenit/backward-to-bol-or-indent)
      (expect (point) :to-equal 10)
      (zenit/backward-to-bol-or-indent)
      (expect (point) :to-equal 12))))


(describe "zenit/forward-to-last-non-comment-or-eol"
  (it "moves point to the last non-comment character on the line"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "Line 1 ;; Comment\nLine 2 ;; Comment\nLine 3 ;; Comment")
      (goto-char 1)
      (zenit/forward-to-last-non-comment-or-eol)
      (expect (point) :to-equal 7)))

  (it "moves point to the end of the line if it's already at the last non-comment character"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "Line 1 ;; Comment\nLine 2 ;; Comment\nLine 3 ;; Comment")
      (goto-char 7)
      (zenit/forward-to-last-non-comment-or-eol)
      (expect (point) :to-equal 18))))


(describe "zenit/delete-backward-word"
  (it "deletes the previous word"
    (with-temp-buffer
      (insert "One two three")
      (goto-char (point-max))
      (zenit/delete-backward-word 1)
      (expect (buffer-string) :to-equal "One two ")))

  (it "deletes the word but keeps whitespace before the cursor"
    (with-temp-buffer
      (insert "One two  three")
      (goto-char (point-max))
      (zenit/delete-backward-word 1)
      (expect (buffer-string) :to-equal "One two  "))))


(describe "zenit/dumb-indent"
  (it "indents the current line correctly"
    (with-temp-buffer
      (insert "First line\nSecond line")
      (goto-char (point-max))
      (beginning-of-line)
      (zenit/dumb-indent)
      (expect (buffer-string) :to-equal "First line\n\tSecond line"))))


(describe "zenit/dumb-dedent"
  (it "dedents the current line correctly"
    (with-temp-buffer
      (insert "First line\n\tSecond line")
      (goto-char (point-max))
      (zenit/backward-to-bol-or-indent)
      (zenit/dumb-dedent)
      (expect (buffer-string) :to-equal "First line\nSecond line"))))


(describe "zenit/retab"
  (it "replaces tabs with spaces correctly"
    (with-temp-buffer
      (insert "First line\n\tSecond line")
      (goto-char (point-min))
      (setq tab-width 4)
      (zenit/retab t)
      (expect (buffer-string) :to-equal "First line\n    Second line"))))


(describe "zenit/delete-trailing-newlines"
  (it "removes trailing newlines"
    (with-temp-buffer
      (insert "First line\nSecond line\n\n\n")
      (zenit/delete-trailing-newlines)
      (expect (buffer-string) :to-equal "First line\nSecond line\n")))

  (it "does not remove inner newlines"
    (with-temp-buffer
      (insert "First line\n\nSecond line\n\nThird line\n\n")
      (zenit/delete-trailing-newlines)
      (expect (buffer-string) :to-equal "First line\n\nSecond line\n\nThird line\n"))))


(describe "zenit/dos2unix"
  (it "changes buffer-file-coding-system correctly"
    (with-temp-buffer
      (insert "First line\nSecond line\nThird line\n")
      (zenit/dos2unix)
      (expect buffer-file-coding-system :to-be 'utf-8-unix))))


(describe "zenit/unix2dos"
  (it "changes buffer-file-coding-system correctly"
    (with-temp-buffer
      (insert "First line\nSecond line\nThird line\n")
      (zenit/unix2dos)
      (expect buffer-file-coding-system :to-be 'utf-8-dos))))


(describe "zenit/toggle-indent-style"
  (it "toggles indent style between spaces and tabs"
    (spy-on 'message)
    (with-temp-buffer
      (zenit/toggle-indent-style)
      (expect indent-tabs-mode :to-be nil)
      (expect 'message :to-have-been-called-with "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces"))
      (zenit/toggle-indent-style)
      (expect indent-tabs-mode :to-be t)
      (expect 'message :to-have-been-called-with "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))))


(describe "zenit/set-indent-width"
  (it "sets the indent width correctly"
    (spy-on 'message)
    (with-temp-buffer
      (setq-default indent-tabs-mode nil)
      (zenit/set-indent-width 2)
      (expect tab-width :to-equal 2)
      (expect 'message :to-have-been-called-with "Changed indentation to %d" 2)
      (zenit/set-indent-width 4)
      (expect tab-width :to-equal 4)
      (expect 'message :to-have-been-called-with "Changed indentation to %d" 4))))


(describe "zenit-enable-delete-trailing-whitespace-h"
  (it "enables ws-butler-mode"
    (spy-on 'ws-butler-mode)
    (with-temp-buffer
      (zenit-enable-delete-trailing-whitespace-h)
      (expect 'ws-butler-mode :to-have-been-called-with +1))))


(describe "zenit-disable-delete-trailing-whitespace-h"
  (it "disables ws-butler-mode"
    (spy-on 'ws-butler-mode)
    (with-temp-buffer
      (zenit-enable-delete-trailing-whitespace-h)
      (expect 'ws-butler-mode :to-have-been-called-with +1))))


(describe "zenit-enable-show-trailing-whitespace-h"
  (it "disables show-trailing-whitespace"
    (with-temp-buffer
      (zenit-enable-show-trailing-whitespace-h)
      (expect show-trailing-whitespace :to-be t))))


(describe "zenit-disable-show-trailing-whitespace-h"
  (it "disables show-trailing-whitespace"
    (with-temp-buffer
      (zenit-disable-show-trailing-whitespace-h)
      (expect show-trailing-whitespace :to-be nil))))

  )
