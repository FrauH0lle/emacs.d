;; lisp/core/cli/eval.el -*- lexical-binding: t; -*-

;;;###autoload
(defun zenit-cli-eval (expression eval-file print-result &optional result-only-p)
  "Evaluate arbitrary Elisp code with properly initialized configuration.

This subcommand works similarly to `emacs --batch --eval' but ensures
that the Zenit Emacs configuration is fully loaded and initialized
before executing the code.

EXPRESSION is a string containing an elisp expression to evaluate.
EVAL-FILE is a file path containing elisp code to evaluate.
PRINT-RESULT, if non-nil, prints the result of the evaluation.

At least one of EXPRESSION or EVAL-FILE must be provided.

Examples:
  # Evaluate an expression and print result
  ./bin/emacs-config eval -e \\='(+ 1 2)' -p

  # Evaluate code from a file
  ./bin/emacs-config eval -f script.el

  # Evaluate an expression without printing
  ./bin/emacs-config eval -e \\='(message \"Hello, world!\")'

  # Both expression and file can be provided
  ./bin/emacs-config eval -f init-script.el -e \\='(run-my-command)' -p"
  (unless (or expression eval-file)
    (print! (error "No expression or file provided"))
    (kill-emacs 1))

  (let (result)
    (print-group!
      ;; Evaluate file if provided
      (when eval-file
        (unless result-only-p
          (print! (start "Loading file: %s") (path eval-file)))
        (unless (file-exists-p eval-file)
          (print! (error "File not found: %s") (path eval-file))
          (kill-emacs 1))
        (condition-case err
            (progn
              (setq result (load eval-file nil nil nil))
              (unless result-only-p
                (print! (success "Successfully loaded file"))))
          (error
           (print! (error "Failed to load file: %s") (error-message-string err))
           (kill-emacs 1))))

      ;; Evaluate expression if provided
      (when expression
        (unless result-only-p
          (print! (start "Evaluating expression: %s") expression))
        (condition-case err
            (progn
              (setq result (eval (read expression) t))
              (unless result-only-p
                (print! (success "Successfully evaluated expression"))))
          (error
           (print! (error "Failed to evaluate expression: %s") (error-message-string err))
           (kill-emacs 1))))

      ;; Print result if requested
      (when (and print-result result)
        (let ((result-str (prin1-to-string result)))
          (if result-only-p
              (print! "%s" result-str)
            (print! (item "Result:"))
            (print-group!
              (if (> (length result-str) 200)
                  ;; For long results, use pp for better formatting
                  (print! "%s" (with-temp-buffer
                                 (pp result (current-buffer))
                                 (buffer-string)))
                ;; For short results, just print directly
                (print! "%s" result-str)))))))))

(provide 'zenit-cli-eval)

;;; eval.el ends here
