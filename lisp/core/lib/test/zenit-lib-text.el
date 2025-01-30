;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-text.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'text)

(zenit-deftest zenit--sppss-memo-last-point
  (:doc "`zenit--sppss-memo-last-point' is defined")
  (should (boundp 'zenit--sppss-memo-last-point)))

(zenit-deftest zenit--sppss-memo-last-result
  (:doc "`zenit--sppss-memo-last-result' is defined")
  (should (boundp 'zenit--sppss-memo-last-result)))

(zenit-deftest zenit--sppss-memo-reset-h
  (:doc "`zenit--sppss-memo-reset-h' is defined")
  (should (fboundp 'zenit--sppss-memo-reset-h)))

(zenit-deftest zenit-syntax-ppss
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer)
     (emacs-lisp-mode)
     (setq zenit--sppss-memo-last-point nil
           zenit--sppss-memo-last-result nil))
   :after-each
   (kill-buffer test-buffer)
   :doc "`zenit-syntax-ppss' parses syntax and caches state")
  ,test
  (test)
  (with-current-buffer test-buffer
    (insert "(hello \"world\") ; comment")
    (goto-char 1)
    ;; First call should compute and cache
    (let ((result1 (zenit-syntax-ppss 8)))
      ;; Second call should return cached result
      (should (eq result1 (zenit-syntax-ppss 8)))))
  (with-current-buffer test-buffer
    (insert "(hello \"world\") ; comment")
    (goto-char 1)
    ;; Test string detection
    (should (nth 3 (zenit-syntax-ppss 9))))
  (with-current-buffer test-buffer
    (insert "(hello \"world\") ; comment")
    (goto-char 1)
    ;; Test comment detection
    (should (nth 4 (zenit-syntax-ppss 19)))))

(zenit-deftest zenit-surrounded-p
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer)
     (emacs-lisp-mode))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-surrounded-p' returns t when point is surrounded by braces"
  (with-current-buffer test-buffer
    (insert "foo {bar} baz")
    (goto-char 7)
    (should (zenit-surrounded-p '(:beg 5 :end 8 :op "{" :cl "}"))))
  
  :doc "`zenit-surrounded-p' returns nil when point is not surrounded by braces"
  (with-current-buffer test-buffer
    (insert "foo {bar} baz")
    (goto-char 4)
    (should-not (zenit-surrounded-p '(:beg 5 :end 8 :op "{" :cl "}"))))
  
  :doc "`zenit-surrounded-p' returns t when point is surrounded by braces on the same line and whitespace is balanced"
  (with-current-buffer test-buffer
    (insert "foo { bar } baz")
    (goto-char 8)
    (should (zenit-surrounded-p '(:beg 5 :end 11 :op "{" :cl "}") t t)))
  
  :doc "`zenit-surrounded-p' returns nil when point is surrounded by braces but whitespace is not balanced"
  (with-current-buffer test-buffer
    (insert "foo {  bar } baz")
    (goto-char 9)
    (should-not (zenit-surrounded-p '(:beg 5 :end 12 :op "{" :cl "}") t t)))
  
  :doc "`zenit-surrounded-p' returns t when point is surrounded by braces on adjacent lines"
  (with-current-buffer test-buffer
    (insert "foo {\n bar\n} baz")
    (goto-char 8)
    (should (zenit-surrounded-p '(:beg 5 :end 12 :op "{" :cl "}"))))
  
  :doc "`zenit-surrounded-p' returns t when point is surrounded by parentheses"
  (with-current-buffer test-buffer
    (insert "foo (bar) baz")
    (goto-char 7)
    (should (zenit-surrounded-p '(:beg 5 :end 8 :op "(" :cl ")")))))

(zenit-deftest zenit-point-in-comment-p
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-point-in-comment-p' returns t when point is within a single-line comment"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "foo ;; this is a comment\nbar")
    (goto-char 10)
    (should (zenit-point-in-comment-p)))

  :doc "`zenit-point-in-comment-p' returns nil when point is not within a single-line comment"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "foo ;; this is a comment\nbar")
    (goto-char 4)
    (should-not (zenit-point-in-comment-p)))

  :doc "`zenit-point-in-comment-p' returns t when point is within a multi-line comment"
  (with-current-buffer test-buffer
    (c-mode)
    (insert "foo\n/* this is a\n multi-line comment */\nbar")
    (goto-char 15)
    (should (zenit-point-in-comment-p)))

  :doc "`zenit-point-in-comment-p' returns nil when point is not within a multi-line comment"
  (with-current-buffer test-buffer
    (c-mode)
    (insert "foo\n/* this is a\n multi-line comment */\nbar")
    (goto-char 4)
    (should-not (zenit-point-in-comment-p))))


(zenit-deftest zenit-point-in-string-p
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer)
     (conf-mode))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-point-in-string-p' returns t when point is within a double-quoted string"
  (with-current-buffer test-buffer
    (insert "foo \"this is a string\" bar")
    (goto-char 10)
    (should (zenit-point-in-string-p)))

  :doc "`zenit-point-in-string-p' returns nil when point is not within a double-quoted string"
  (with-current-buffer test-buffer
    (insert "foo \"this is a string\" bar")
    (goto-char 5)
    (should-not (zenit-point-in-string-p)))

  :doc "`zenit-point-in-string-p' returns t when point is within a single-quoted string"
  (with-current-buffer test-buffer
    (insert "foo 'this is a string' bar")
    (goto-char 10)
    (should (zenit-point-in-string-p)))

  :doc "`zenit-point-in-string-p' returns nil when point is not within a single-quoted string"
  (with-current-buffer test-buffer
    (insert "foo 'this is a string' bar")
    (goto-char 5)
    (should-not (zenit-point-in-string-p))))

(zenit-deftest zenit-point-in-string-or-comment-p
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-point-in-string-or-comment-p' returns t when point is within a double-quoted string"
  (with-current-buffer test-buffer
    (conf-mode)
    (insert "foo \"this is a string\" bar")
    (goto-char 10)
    (should (zenit-point-in-string-or-comment-p)))

  :doc "`zenit-point-in-string-or-comment-p' returns t when point is within a single-quoted string"
  (with-current-buffer test-buffer
    (conf-mode)
    (insert "foo 'this is a string' bar")
    (goto-char 10)
    (should (zenit-point-in-string-or-comment-p)))

  :doc "`zenit-point-in-string-or-comment-p' returns t when point is within a comment"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "foo ; this is a comment\nbar")
    (goto-char 10)
    (should (zenit-point-in-string-or-comment-p)))

  :doc "`zenit-point-in-string-or-comment-p' returns nil when point is not within a string or comment"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "foo \"this is a string\" bar")
    (goto-char 5)
    (should-not (zenit-point-in-string-or-comment-p))))

(zenit-deftest zenit-region-active-p
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-region-active-p' returns t when the region is active"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 1)
    (set-mark 5)
    (activate-mark)
    (should (zenit-region-active-p)))

  :doc "`zenit-region-active-p' returns nil when the region is inactive"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 1)
    (set-mark 5)
    (deactivate-mark)
    (should-not (zenit-region-active-p))))

(zenit-deftest zenit-region-beginning
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-region-beginning' returns the beginning of the active region"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 1)
    (set-mark 5)
    (activate-mark)
    (goto-char 9)
    (should (equal 5 (zenit-region-beginning)))))

(zenit-deftest zenit-region-end
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-region-end' returns the end of the active region"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 1)
    (set-mark 5)
    (activate-mark)
    (goto-char 9)
    (should (equal 9 (zenit-region-end)))))

(zenit-deftest zenit-thing-at-point-or-region
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit-thing-at-point-or-region' returns the text in the active region"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 1)
    (set-mark 6)
    (activate-mark)
    (goto-char 9)
    (should (equal "is " (zenit-thing-at-point-or-region 'symbol))))
  :doc "`zenit-thing-at-point-or-region' returns the thing at point when the region is inactive"
  (with-current-buffer test-buffer
    (insert "This is a test")
    (goto-char 6)
    (deactivate-mark)
    (should (equal "is" (zenit-thing-at-point-or-region 'symbol)))))

(zenit-deftest zenit--bol-bot-eot-eol
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit--bol-bot-eot-eol' returns beginning of line, beginning of top line, end of top line, and end of line correctly"
  (with-current-buffer test-buffer
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char 10)
    (let ((result (zenit--bol-bot-eot-eol)))
      ;; beginning of line
      (should (equal 8 (nth 0 result)))
      ;; beginning of top line
      (should (equal 8 (nth 1 result)))
      ;; end of top line
      (should (equal 14 (nth 2 result)))
      ;; end of line
      (should (equal 14 (nth 3 result))))))

(zenit-deftest zenit--last-backward-pt
  (:doc "`zenit--last-backward-pt' is defined")
  (should (boundp 'zenit--last-backward-pt)))

(zenit-deftest zenit/backward-to-bol-or-indent
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/backward-to-bol-or-indent' moves point to the first non-whitespace character on the line"
  (with-current-buffer test-buffer
    (insert "  Line 1\n  Line 2\n  Line 3")
    (goto-char 12)
    (zenit/backward-to-bol-or-indent)
    (should (equal 10 (point))))
  :doc "`zenit/backward-to-bol-or-indent' moves point to the beginning of the line if it's already at the first non-whitespace character"
  (with-current-buffer test-buffer
    (insert "  Line 1\n  Line 2\n  Line 3")
    (goto-char 12)
    (zenit/backward-to-bol-or-indent)
    (should (equal 10 (point)))
    (zenit/backward-to-bol-or-indent)
    (should (equal 12 (point)))))

(zenit-deftest zenit--last-forward-pt
  (:doc "`zenit--last-forward-pt' is defined")
  (should (boundp 'zenit--last-forward-pt)))

(zenit-deftest zenit/forward-to-last-non-comment-or-eol
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/forward-to-last-non-comment-or-eol' moves point to the last non-comment character on the line"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "Line 1 ;; Comment\nLine 2 ;; Comment\nLine 3 ;; Comment")
    (goto-char 1)
    (zenit/forward-to-last-non-comment-or-eol)
    (should (equal 7 (point))))
  :doc "`zenit/forward-to-last-non-comment-or-eol' moves point to the end of the line if it's already at the last non-comment character"
  (with-current-buffer test-buffer
    (emacs-lisp-mode)
    (insert "Line 1 ;; Comment\nLine 2 ;; Comment\nLine 3 ;; Comment")
    (goto-char 7)
    (zenit/forward-to-last-non-comment-or-eol)
    (should (equal 18 (point)))))

(zenit-deftest zenit/delete-backward-word
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/delete-backward-word' deletes the previous word"
  (with-current-buffer test-buffer
    (insert "One two three")
    (goto-char (point-max))
    (zenit/delete-backward-word 1)
    (should (equal "One two " (buffer-string))))
  :doc "`zenit/delete-backward-word' deletes the word but keeps whitespace before the cursor"
  (with-current-buffer test-buffer
    (insert "One two  three")
    (goto-char (point-max))
    (zenit/delete-backward-word 1)
    (should (equal "One two  " (buffer-string)))))

(zenit-deftest zenit/dumb-indent
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/dumb-indent' indents the current line correctly"
  (with-current-buffer test-buffer
    (insert "First line\nSecond line")
    (goto-char (point-max))
    (beginning-of-line)
    (zenit/dumb-indent)
    (should (equal "First line\n\tSecond line" (buffer-string)))))

(zenit-deftest zenit/dumb-dedent
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/dumb-dedent' dedents the current line correctly"
  (with-current-buffer test-buffer
    (insert "First line\n\tSecond line")
    (goto-char (point-max))
    (zenit/backward-to-bol-or-indent)
    (zenit/dumb-dedent)
    (should (equal "First line\nSecond line" (buffer-string)))))

(zenit-deftest zenit/retab
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/retab' replaces tabs with spaces correctly"
  (with-current-buffer test-buffer
    (insert "First line\n\tSecond line")
    (goto-char (point-min))
    (setq tab-width 4)
    (zenit/retab t)
    (should (equal "First line\n    Second line" (buffer-string)))))

(zenit-deftest zenit/delete-trailing-newlines
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/delete-trailing-newlines' removes trailing newlines but keeps one"
  (with-current-buffer test-buffer
    (insert "First line\nSecond line\n\n\n")
    (zenit/delete-trailing-newlines)
    (should (equal "First line\nSecond line\n" (buffer-string))))
  :doc "`zenit/delete-trailing-newlines' does not remove inner newlines"
  (with-current-buffer test-buffer
    (insert "First line\n\nSecond line\n\nThird line\n\n")
    (zenit/delete-trailing-newlines)
    (should (equal "First line\n\nSecond line\n\nThird line\n" (buffer-string)))))

(zenit-deftest zenit/dos2unix
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/dos2unix' changes buffer-file-coding-system to unix format"
  (with-current-buffer test-buffer
    (insert "First line\nSecond line\nThird line\n")
    (zenit/dos2unix)
    (should (eq buffer-file-coding-system 'utf-8-unix))))

(zenit-deftest zenit/unix2dos
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/unix2dos' changes buffer-file-coding-system to dos format"
  (with-current-buffer test-buffer
    (insert "First line\nSecond line\nThird line\n")
    (zenit/unix2dos)
    (should (eq buffer-file-coding-system 'utf-8-dos))))

(zenit-deftest zenit/toggle-indent-style
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/toggle-indent-style' toggles indent style between spaces and tabs"
  (with-current-buffer test-buffer
    (let ((current-val indent-tabs-mode))
      (insert "First line\nSecond line\nThird line\n")
      (quiet!!
        (zenit/toggle-indent-style))
      (should-not (equal current-val indent-tabs-mode))
      (quiet!!
        (zenit/toggle-indent-style))
      (should (equal current-val indent-tabs-mode)))))

(zenit-deftest zenit/set-indent-width
  (:vars ((test-buffer (get-buffer-create "test-buffer")))
   :before-each
   (with-current-buffer test-buffer
     (erase-buffer))
   :after-each
   (kill-buffer test-buffer))
  ,test
  (test)
  :doc "`zenit/set-indent-width' sets the indent width correctly"
  (with-current-buffer test-buffer
    (setq-default indent-tabs-mode nil)
    (quiet!!
      (zenit/set-indent-width 2))
    (should (equal 2 tab-width))
    (quiet!!
      (zenit/set-indent-width 4))
    (should (equal 4 tab-width))))

(zenit-deftest zenit-enable-delete-trailing-whitespace-h
  (:doc "`zenit-enable-delete-trailing-whitespace-h' is defined")
  (should (fboundp 'zenit-enable-delete-trailing-whitespace-h)))

(zenit-deftest zenit-disable-delete-trailing-whitespace-h
  (:doc "`zenit-disable-delete-trailing-whitespace-h' is defined")
  (should (fboundp 'zenit-disable-delete-trailing-whitespace-h)))

(zenit-deftest zenit-enable-show-trailing-whitespace-h
  (:doc "`zenit-enable-show-trailing-whitespace-h' is defined")
  (should (fboundp 'zenit-enable-show-trailing-whitespace-h)))

(zenit-deftest zenit-disable-show-trailing-whitespace-h
  (:doc "`zenit-disable-show-trailing-whitespace-h' is defined")
  (should (fboundp 'zenit-disable-show-trailing-whitespace-h)))
