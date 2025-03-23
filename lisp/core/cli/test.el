;; lisp/core/cli/test.el -*- lexical-binding: t; -*-

(defun zenit--emacs-binary ()
  "Return the path to the Emacs executable to use for testing.
On Windows, prefers runemacs.exe if available, otherwise falls
back to the standard Emacs executable. On other systems, returns
the standard Emacs binary."
  ;; Construct path to standard Emacs binary using invocation directory and name
  (let ((emacs-binary-path (zenit-path invocation-directory invocation-name))
        ;; On Windows, also check for runemacs.exe in the same directory
        (runemacs-binary-path (if zenit--system-windows-p 
                                  (zenit-path invocation-directory "runemacs.exe"))))
    ;; Prefer runemacs.exe on Windows if it exists, otherwise use standard Emacs
    (if (and runemacs-binary-path (file-exists-p runemacs-binary-path))
        runemacs-binary-path
      emacs-binary-path)))

(defun zenit-cli--tests-cleanup ()
  "Clean up temporary files and directories after test execution.

Specifically removes:
- Projectile bookmarks file
- ELN cache directories

This ensures a clean state for subsequent test runs."
  ;; Find all files that need to be cleaned up
  (let ((artifact-files
         (zenit-glob zenit-emacs-dir "projectile-bookmarks.eld"))
        (artifact-dirs
         (zenit-glob zenit-emacs-dir "eln-cache")))
    ;; Delete each artifact file if it exists
    (dolist (f artifact-files)
      (ignore-errors (delete-file f)))
    ;; Delete each artifact directory if it exists (recursively)
    (dolist (d artifact-dirs)
      (ignore-errors (delete-directory d t)))))

(defun zenit-cli-test (&optional files)
  "Run unit tests for Zenit Emacs configuration.

If FILES is provided, it should be a list of test files to run.
If not provided, all test files in the core test directory will
be run.

The function:
1. Runs tests in batch mode using a clean Emacs instance
2. Captures and processes the output
3. Reports test results with detailed failure information
4. Cleans up test artifacts
5. Exits with status code 1 if any tests fail

Returns nil, but may exit Emacs with non-zero status if tests
fail."
  (require 'ansi-color)
  (print! (start "Running unit tests..."))
  (print-group!
    (with-temp-buffer
      ;; Get list of test files to run (either provided or all in test directory)
      (let ((files (or (when files
                         (cl-loop for file in files
                                  collect (cons (zenit-module-from-path file) file)))
                       (append (cl-loop for file in (zenit-glob zenit-core-dir "lib/test/*.el")
                                        collect (cons (cons :core nil) file))
                               (cl-loop for (cat . mod) in (zenit-module-list 'all)
                                        append (cl-loop for file in (zenit-glob (zenit-module-locate-path cat mod) "test" "*.el")
                                                        collect (cons (cons cat mod) file))))))
            read-files)
        ;; Run each test file in a clean Emacs process
        (cl-loop for ((_cat . _mod) . file) in files
                 do
                 (cl-destructuring-bind (_status . output)
                     (apply #'zenit-exec-process
                            (zenit--emacs-binary)
                            ;; Start with no init file
                            "-Q"
                            ;; Run in batch mode
                            "--batch"
                            ;; Set init directory
                            (concat "--init-directory=" (expand-file-name user-emacs-directory))
                            ;; Always load newest files
                            "--eval" (prin1-to-string '(setq load-prefer-newer t))
                            ;; Load core
                            "-l" (concat zenit-core-dir "zenit-core.el")
                            ;; Set context
                            "--eval" (prin1-to-string '(zenit-context-push 'init))
                            ;; Load `zenit-cache-generators'
                            "-l" (file-name-concat zenit-local-dir (car (mapcar #'car zenit-cache-generators)))
                            ;; Set test context
                            "--eval" (prin1-to-string '(zenit-context-pop 'init))
                            "--eval" (prin1-to-string '(zenit-context-push 'tests))
                            ;; Load test framework
                            "-l" (concat zenit-core-dir "zenit-test.el")
                            ;; Load test file
                            `("-l" ,file
                              ;; Run tests and exit
                              "-f" "ert-run-tests-batch-and-exit"))
                   ;; Remove ANSI color codes from output and insert into buffer
                   (insert (replace-regexp-in-string ansi-color-control-seq-regexp "" output))
                   ;; Track which files we've processed
                   (push file read-files)))

        (setq read-files (nreverse read-files))
        ;; Initialize counters for test results
        (let ((total 0)
              (total-success 0)
              (total-failed 0)
              (total-skipped 0)
              (i 0)
              ;; last-fail tracks the position in the buffer where we last
              ;; processed failed test results. This helps avoid reprocessing
              ;; the same sections when searching for failed test conditions.
              last-fail)
          (print! "----------------------------------------\nTests finished")
          (print-group!
            (goto-char (point-min))
            ;; Regex to match test summary lines:
            ;; "^Ran \\([0-9]+\\) tests, \\([0-9]+\\) results as expected, \\([0-9]+\\) unexpected"
            ;; - Captures total tests run, successful tests, and failed tests
            (while (re-search-forward "^[[:space:]]*Ran \\(?1:[0-9]+\\) tests, \\(?2:[0-9]+\\) results as expected, \\(?3:[0-9]+\\) unexpected\\(?:, \\(?4:[0-9]+\\) skipped\\)?" nil t)
              (let ((ran (string-to-number (match-string 1)))
                    (success (string-to-number (match-string 2)))
                    (failed (string-to-number (match-string 3)))
                    (skipped (if (match-string 4) (string-to-number (match-string 4)) 0)))
                (when (> failed 0)
                  (terpri)
                  (print! (warn "(%s) Failed %d/%d tests")
                          (path (nth i read-files))
                          failed ran)
                  ;; Find all failed test names
                  (save-excursion
                    (let (failed-tests
                          ;; Find boundary of failure section using regex:
                          ;; "^[[:space:]]*\\([0-9]+\\) unexpected results:"
                          ;; This prevents searching beyond the relevant test
                          ;; results when looking for failure details.
                          (bound (re-search-forward "^[[:space:]]*\\([0-9]+\\) unexpected results:" nil t)))
                      ;; Collect all failed test names using regex:
                      ;; "^[[:space:]]*FAILED[[:space:]]+\\(.*?\\)\\(?:[[:space:]]\\|Running\\|$\\)"
                      ;; - Matches FAILED followed by test name until space, "Running" or end of line
                      (dotimes (_ failed)
                        (re-search-forward
                         "^[[:space:]]*FAILED[[:space:]]+\\(.*?\\)\\(?:[[:space:]]\\|Running\\|$\\)"
                         nil t)
                        (push (match-string 1) failed-tests))
                      ;; For each failed test, find and print its condition
                      (print-group!
                        (dolist (test (nreverse failed-tests))
                          (save-excursion
                            (goto-char (or last-fail (point-min)))
                            ;; Search for test condition using regex:
                            ;; "Test " + quoted test name + " condition:"
                            (when (re-search-forward (concat "Test " (regexp-quote test) " condition:") bound t)
                              ;; Find end of condition using regex:
                              ;; "^[[:space:]]*\\(?:FAILED\\|passed\\)"
                              ;; - Matches either FAILED or passed at start of line
                              (let ((start (match-beginning 0))
                                    (end (save-excursion
                                           (or (and (re-search-forward "^[[:space:]]*\\(?:FAILED\\|passed\\)" nil t)
                                                    (match-beginning 0))
                                               (point-max)))))
                                (print! "\n%s" (string-trim
                                                (buffer-substring start end))))))))))
                  (setq last-fail (point)))
                (when (> skipped 0)
                  (terpri)
                  (print! (item "(%s) Skipped %d/%d tests")
                          (path (nth i read-files))
                          skipped ran)
                  ;; Find all skipped test names
                  (save-excursion
                    (let (skipped-tests)
                      ;; Collect all skipped test names using regex:
                      ;; "^[[:space:]]*SKIPPED[[:space:]]+\\(.*?\\)\\(?:[[:space:]]\\|Running\\|$\\)"
                      ;; - Matches SKIPPED followed by test name until space, "Running" or end of line
                      (dotimes (_ skipped)
                        (re-search-forward
                         "^[[:space:]]*SKIPPED[[:space:]]+\\(.*?\\)\\(?:[[:space:]]\\|Running\\|$\\)"
                         nil t)
                        (push (match-string 1) skipped-tests))
                      ;; For each failed test, find and print its condition
                      (print-group!
                        (dolist (test (nreverse skipped-tests))
                          (print! "\n%s" (string-trim test)))))))
                ;; Update totals
                (cl-incf total ran)
                (cl-incf total-success success)
                (cl-incf total-failed failed)
                (cl-incf total-skipped skipped)
                (cl-incf i))))
          (terpri)
          ;; Clean up test artifacts
          (zenit-cli--tests-cleanup)
          ;; Print final results
          (if (= total-failed 0)
              (print! (success "Ran %d/%d tests successfully. %d tests skipped." total-success total total-skipped))
            (print! (error "Ran %d tests, %d tests skipped, %d failed.") total total-skipped total-failed)
            (kill-emacs 1)))))))
