;; lisp/core/lib/format.el -*- lexical-binding: t; -*-

(defvar zenit-format-ansi-alist
  '(;; fx
    (bold       1 :weight bold)
    (dark       2)
    (italic     3 :slant italic)
    (underscore 4 :underline t)
    (blink      5)
    (rapid      6)
    (contrary   7)
    (concealed  8)
    (strike     9 :strike-through t)
    ;; fg
    (black      30 term-color-black)
    (red        31 term-color-red)
    (green      32 term-color-green)
    (yellow     33 term-color-yellow)
    (blue       34 term-color-blue)
    (magenta    35 term-color-magenta)
    (cyan       36 term-color-cyan)
    (white      37 term-color-white)
    ;; bg
    (on-black   40 term-color-black)
    (on-red     41 term-color-red)
    (on-green   42 term-color-green)
    (on-yellow  43 term-color-yellow)
    (on-blue    44 term-color-blue)
    (on-magenta 45 term-color-magenta)
    (on-cyan    46 term-color-cyan)
    (on-white   47 term-color-white))
  "An alist of fg/bg/fx names mapped to ansi codes and term-color-* variables.
This serves as the cipher for converting (COLOR ...) function
calls in `print!' and `format!' into colored output, where COLOR
is any car of this list.")

(defvar zenit-format-class-alist
  `((color . zenit--format-color)
    (class . zenit--format-class)
    (indent . zenit--format-indent)
    (autofill . zenit--format-autofill)

    (success . (lambda (str &rest args)
                 (apply #'zenit--format-color 'green (format "✓ %s" str) args)))
    (warn    . (lambda (str &rest args)
                 (apply #'zenit--format-color 'yellow (format "! %s" str) args)))
    (error   . (lambda (str &rest args)
                 (apply #'zenit--format-color 'red (format "x %s" str) args)))
    (info    . (lambda (str &rest args)
                 (concat "- " (if args (apply #'format str args) str))))
    (start    . (lambda (str &rest args)
                  (concat "> " (if args (apply #'format str args) str))))
    (debug   . (lambda (str &rest args)
                 (if init-file-debug
                     (if args
                         (apply #'format str args)
                       (format "%s" str))
                   "")))
    (path    . abbreviate-file-name)
    (symbol . symbol-name)
    (relpath . (lambda (str &optional dir)
                 (if (or (not str)
                         (not (stringp str))
                         (string-empty-p str))
                     str
                   (let ((dir (or dir (file-truename default-directory)))
                         (str (file-truename str)))
                     (if (file-in-directory-p str dir)
                         (file-relative-name str dir)
                       (abbreviate-file-name str))))))
    (filename . file-name-nondirectory)
    (dirname . (lambda (path)
                 (unless (file-directory-p path)
                   (setq path (file-name-directory path)))
                 (directory-file-name path))))
  "An alist of text classes that map to transformation functions.
Any of these classes can be called like functions from within
`format!' and `print!' calls, which will transform their input.")

(defvar zenit-format-indent 0
  "Level to rigidly indent text returned by `format!' and `print!'.")

(defvar zenit-format-indent-increment 2
  "Steps in which to increment `zenit-format-indent' for
consecutive levels.")

(defvar zenit-format-backend
  (if noninteractive 'ansi 'text-properties)
  "Determines whether to print colors with ANSI codes or with text
properties. Accepts \\='ansi and \\='text-properties. nil means
don't render colors.")

;;; Library

;;;###autoload
(defun zenit--format (output)
  "Format OUTPUT by indenting it according to `zenit-format-indent'.
The function indents each line in OUTPUT by the number of spaces
specified in `zenit-format-indent'. Empty lines are ignored."
  (if (string-empty-p (string-trim output))
      ""
    (let ((indentation (make-string zenit-format-indent 32)))
      (concat indentation
              (replace-regexp-in-string
               "\n"
               (concat "\n" indentation)
               output t t)))))

(defun zenit--format-print (output)
  "Print the formatted OUTPUT string.

When in an interactive session, use `message' to display OUTPUT.
In non-interactive sessions, use `princ' to print OUTPUT to
standard output, followed by a newline.

If OUTPUT is empty, simply returns nil. Else it returns t after
printing."
  (unless (string-empty-p output)
    (if (not noninteractive)
        ;; Display output in interactive sessions
        (message "%s" output)
      ;; Print output to standard output in non-interactive sessions
      (princ output)
      ;; Add a newline
      (terpri))
    t))

;;;###autoload
(defun zenit--format-indent (width text &optional prefix)
  "Indent TEXT by WIDTH spaces.
 If PREFIX is provided, add it before each line of the indented
 text if WIDTH is greater than 2.

The resulting text is then returned as a string."
  (with-temp-buffer
    (setq text (format "%s" text))
    (insert text)
    (indent-rigidly (point-min) (point-max) width)
    (when (stringp prefix)
      (when (> width 2)
        (goto-char (point-min))
        (beginning-of-line-text)
        (delete-char (- (length prefix)))
        (insert prefix)))
    (buffer-string)))

;;;###autoload
(defun zenit--format-autofill (&rest msgs)
  "Ensure MSG is split into lines no longer than `fill-column'."
  (with-temp-buffer
    (let ((fill-column 76))
      (dolist (line msgs)
        (when line
          (insert (format "%s" line))))
      (fill-region (point-min) (point-max))
      (buffer-string))))

;;;###autoload
(defun zenit--format-color (style format &rest args)
  "Apply STYLE to formatted message FORMAT with ARGS.
STYLE is a symbol that correlates to `zenit-format-ansi-alist'.
In a noninteractive session, this wraps the result in ansi color
codes. Otherwise, it maps colors to a term-color-* face."
  (let* ((code (cadr (assq style zenit-format-ansi-alist)))
         (format (format "%s" format))
         (message (if args (apply #'format format args) format)))
    (unless code
      (error "%S is an invalid color" style))
    (pcase zenit-format-backend
      (`ansi
       (format "\e[%dm%s\e[%dm" code message 0))
      (`text-properties
       (require 'term)  ; piggyback on term's color faces
       (propertize
        message
        'face
        (append (get-text-property 0 'face format)
                (cond ((>= code 40)
                       `(:background ,(caddr (assq style zenit-format-ansi-alist))))
                      ((>= code 30)
                       `(:foreground ,(face-foreground (caddr (assq style zenit-format-ansi-alist)))))
                      ((cddr (assq style zenit-format-ansi-alist)))))))
      (_ message))))

;;;###autoload
(defun zenit--format-class (class format &rest args)
  "Apply CLASS to formatted format with ARGS.
CLASS is derived from `zenit-format-class-alist', and can
contain any arbitrary, transformative logic."
  (let (fn)
    (cond ((setq fn (cdr (assq class zenit-format-class-alist)))
           (if (functionp fn)
               (apply fn format args)
             (error "%s does not have a function" class)))
          (args (apply #'format format args))
          (format))))

;;;###autoload
(defun zenit--format-apply (forms &optional sub)
  "Replace color-name functions with calls to
`zenit--format-color'."
  (cond ((null forms) nil)
        ((listp forms)
         (append (cond ((not (symbolp (car forms)))
                        (list (zenit--format-apply (car forms))))
                       (sub
                        (list (car forms)))
                       ((assq (car forms) zenit-format-ansi-alist)
                        `(zenit--format-color ',(car forms)))
                       ((assq (car forms) zenit-format-class-alist)
                        `(zenit--format-class ',(car forms)))
                       ((list (car forms))))
                 (zenit--format-apply (cdr forms) t)
                 nil))
        (forms)))

;;;###autoload
(defmacro format! (message &rest args)
  "An alternative to `format' that understands (color ...) and
converts them into faces or ANSI codes depending on the type of
sesssion we're in."
  `(zenit--format (format ,@(zenit--format-apply `(,message ,@args)))))

;;;###autoload
(defmacro print-group! (&rest body)
  "Indents any `print!' or `format!' output within BODY."
  (declare (debug (body)))
  `(let ((zenit-format-indent (+ zenit-format-indent-increment zenit-format-indent)))
     ,@body))

;;;###autoload
(defmacro print! (message &rest args)
  "Uses `message' in interactive sessions and `princ'
otherwise (prints to standard out). Can be colored using (color
...) blocks:
  (print! \"Hello %s\" (bold (blue \"How are you?\")))
  (print! \"Hello %s\" (red \"World\"))
  (print! (green \"Great %s!\") \"success\")
Uses faces in interactive sessions and ANSI codes otherwise."
  `(zenit--format-print (format! ,message ,@args)))

;;;###autoload
(defmacro insert! (message &rest args)
  "Like `insert'; the last argument must be format arguments for MESSAGE.
\(fn MESSAGE... ARGS)"
  `(insert (format! (concat ,message ,@(butlast args))
                    ,@(car (last args)))))

;;;###autoload
(defmacro error! (message &rest args)
  "Like `error', but with the power of `format!'."
  `(error (format! ,message ,@args)))

;;;###autoload
(defmacro user-error! (message &rest args)
  "Like `user-error', but with the power of `format!'."
  `(user-error (format! ,message ,@args)))
