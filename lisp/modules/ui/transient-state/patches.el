;; ui/transient-state/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'symbol-overlay))

;; PATCH 2024-12-12: Store scope bounds in `+symbol-overlay-scope-range'
(el-patch-defun symbol-overlay-narrow (scope &optional window)
  "Narrow to a specific region.
Region might be current scope or displayed window,
depending on SCOPE and WINDOW."
  (if scope
      (let ((pt (point))
            min max p)
        (save-excursion
          (save-restriction
            (narrow-to-defun)
            (setq min (point-min)
                  max (point-max)
                  p (or (/= pt (point)) (= pt (point-max))))))
        (save-excursion
          (and p (setq min (progn (backward-paragraph) (point))
                       max (progn (forward-paragraph) (point))))
          (el-patch-add
            (setq +symbol-overlay-scope-range (cons min max)))
          (narrow-to-region min max)))
    (when (and window (eq (window-buffer) (current-buffer)))
      (el-patch-add
        (setq +symbol-overlay-scope-range (cons (window-start) (window-end))))
      (narrow-to-region (window-start) (window-end)))
    (el-patch-add
      (setq +symbol-overlay-scope-range nil))))

;; PATCH 2024-12-12: Rescpect scope bounds in `+symbol-overlay-scope-range' when
;;   jumping to next match
(el-patch-defun symbol-overlay-basic-jump (symbol dir)
  "Jump to SYMBOL's next location in the direction DIR.
DIR must be non-zero."
  (let* ((case-fold-search nil)
         (bounds (bounds-of-thing-at-point 'symbol))
         (offset (- (point) (if (> dir 0) (cdr bounds) (car bounds))))
         target
         (re (symbol-overlay-regexp symbol))
         (el-patch-add
           (limit (if (> dir 0)
                      (cdr +symbol-overlay-scope-range)
                    (car +symbol-overlay-scope-range)))))
    (goto-char (- (point) offset))
    (setq target (re-search-forward re (el-patch-swap nil (when symbol-overlay-scope limit)) t dir))
    (unless target
      (goto-char (if (> dir 0) (el-patch-swap
                                 (point-min)
                                 (or (car +symbol-overlay-scope-range) (point-min)))
                   (el-patch-swap
                     (point-max)
                     (or (cdr +symbol-overlay-scope-range) (point-max)))))
      (setq target (re-search-forward re (el-patch-swap nil (when symbol-overlay-scope limit)) nil dir)))
    (goto-char (+ target offset))))
