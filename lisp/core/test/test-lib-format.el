;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-format.el

(describe "core/lib/format"

  (load! "lib/format" zenit-core-dir)


  (describe "zenit--format"
    (it "should return an empty string when provided with an empty or whitespace-only string"
      (expect (zenit--format "") :to-equal "")
      (expect (zenit--format " ") :to-equal "")
      (expect (zenit--format "\t") :to-equal "")
      (expect (zenit--format "\n") :to-equal ""))

    (it "should not change the input string when zenit-format-indent is 0"
      (let ((zenit-format-indent 0))
        (expect (zenit--format "Hello, world!") :to-equal "Hello, world!")
        (expect (zenit--format "Multiple\nlines") :to-equal "Multiple\nlines")))

    (it "should indent the input string according to zenit-format-indent"
      (let ((zenit-format-indent 2))
        (expect (zenit--format "Hello, world!") :to-equal "  Hello, world!")
        (expect (zenit--format "Multiple\nlines") :to-equal "  Multiple\n  lines")))

    (it "should work with different values of zenit-format-indent"
      (let ((zenit-format-indent 4))
        (expect (zenit--format "Hello, world!") :to-equal "    Hello, world!")
        (expect (zenit--format "Multiple\nlines") :to-equal "    Multiple\n    lines"))))


  (describe "zenit--format-print"
    (before-each
      (setq noninteractive nil))

    (after-all
      (setq noninteractive t))

    (it "returns nil for an empty string"
      (let ((inhibit-message t))
        (expect (zenit--format-print "") :to-be nil)))

    (it "returns t for a non-empty string"
      (let ((inhibit-message t))
        (expect (zenit--format-print "foo") :to-be t)))

    (it "outputs the string with `message' in an interactive session"
      (spy-on 'message)
      (zenit--format-print "foo")
      (expect 'message :to-have-been-called-with "%s" "foo"))

    (it "outputs the string with `princ' in a non-interactive session"
      (spy-on 'princ)
      (setq noninteractive t)
      (zenit--format-print "foo")
      (expect 'princ :to-have-been-called-with "foo"))

    (it "outputs a newline with `terpri' in a non-interactive session"
      (spy-on 'terpri)
      (setq noninteractive t)
      (zenit--format-print "foo")
      (expect 'terpri :to-have-been-called)))


  (describe "zenit--format-indent"
    (it "indents the text with the specified width"
      (let ((input-text "This is a test\nwith multiple lines")
            (expected-output "  This is a test\n  with multiple lines"))
        (expect (zenit--format-indent 2 input-text) :to-equal expected-output)))

    (it "does not indent when the width is zero"
      (let ((input-text "Another test\nwith different lines"))
        (expect (zenit--format-indent 0 input-text) :to-equal input-text)))

    (it "indents with a prefix when provided"
      (let ((input-text "This is a test\nwith multiple lines")
            (expected-output " > This is a test\n   with multiple lines"))
        (expect (zenit--format-indent 3 input-text "> ") :to-equal expected-output)))

    (it "does not modify the original text"
      (let ((input-text "Original text\nshould not change"))
        (zenit--format-indent 4 input-text)
        (expect input-text :to-equal "Original text\nshould not change"))))


  (describe "zenit--format-autofill"
    (it "does not modify a short string"
      (let ((input "This is a short string.")
            (expected "This is a short string."))
        (expect (zenit--format-autofill input) :to-equal expected)))

    (it "wraps a long string to the specified fill-column (76 characters)"
      (let ((input "This is a long string that should be wrapped according to the specified fill-column.")
            (expected "This is a long string that should be wrapped according to the specified\nfill-column."))
        (expect (zenit--format-autofill input) :to-equal expected)))

    (it "does not preserves existing line breaks"
      (let ((input "This is a string\nwith a line break.")
            (expected "This is a string with a line break."))
        (expect (zenit--format-autofill input) :to-equal expected)))

    (it "handles empty strings"
      (let ((input "")
            (expected ""))
        (expect (zenit--format-autofill input) :to-equal expected)))

    (it "handles strings with only whitespace"
      (let ((input " \t")
            (expected " \t"))
        (expect (zenit--format-autofill input) :to-equal expected))))


  (describe "zenit--format-color"
    (it "returns a string with ANSI escape code for foreground color"
      (expect (zenit--format-color 'red "hello")
              :to-equal "\x1b[31mhello\x1b[0m"))

    (it "returns a string with ANSI escape code for background color"
      (expect (zenit--format-color 'on-red "hello")
              :to-equal "\x1b[41mhello\x1b[0m"))

    (it "throws an error when called with an unknown color"
      (expect (zenit--format-color 'unknown "hello")
              :to-throw 'error))

    (it "returns an empty string when called with an empty input string"
      (expect (zenit--format-color 'red "")
              :to-equal "\x1b[31m\x1b[0m")))


  (describe "zenit--format-class"
    (it "returns the string with a class format applied"
      (expect (zenit--format-class 'warn "foo")
              :to-equal (zenit--format-color 'yellow (format "! %s" "foo"))))

    (it "returns the string unmodified for an unsupported class"
      (expect (zenit--format-class 'unsupported "foo")
              :to-equal "foo")))


  (describe "zenit--format-apply"
    (it "applies color formatting"
      (expect (zenit--format-apply '(red "test")) :to-equal '(zenit--format-color 'red "test")))

    (it "applies class formatting"
      (expect (zenit--format-apply '(warn "test")) :to-equal '(zenit--format-class 'warn "test"))))


  (describe "format!"
    (it "formats and prints a simple string"
      (let ((inhibit-message t))
        (expect (format! "Hello, world!") :to-equal "Hello, world!")))

    (it "formats and prints a string with a variable"
      (let ((inhibit-message t))
        (expect (format! "Hello, %s!" "Emacs") :to-equal "Hello, Emacs!")))

    (it "formats and prints a string with multiple variables"
      (let ((inhibit-message t))
        (expect (format! "%s %d %s" "Answer" 42 "found") :to-equal "Answer 42 found"))))


  (describe "print-group!"
    (it "sets correct indentation level"
      (expect '(print-group! (format! "Hello, World!"))
              :to-expand-into '(let
                                   ((zenit-format-indent
                                     (+ zenit-format-indent-increment zenit-format-indent)))
                                 (format! "Hello, World!")))))


  (describe "print!"
    (it "expands into zenit--format-print"
      (expect '(print! "Hello, %s!" (red "World"))
              :to-expand-into '(zenit--format-print
                                (format! "Hello, %s!"
                                         (red "World"))))))


  (describe "insert!"
    (it "expands into an insert call"
      (expect '(insert! "Hello, World!")
              :to-expand-into '(insert
                                (format!
                                 (concat "Hello, World!"))))))


  (describe "error!"
    (it "expands into an error call"
      (expect '(error! "Hello, World!")
              :to-expand-into '(error
                                (format! "Hello, World!")))))


  (describe "user-error!"
    (it "expands into an user-error call"
      (expect '(user-error! "Hello, World!")
              :to-expand-into '(user-error
                                (format! "Hello, World!"))))))
