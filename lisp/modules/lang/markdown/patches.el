;; lang/markdown/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'markdown-mode))

;; PATCH 2026-02-10: See
;;   https://github.com/jrblevin/markdown-mode/issues/724#issue-1322898756
(el-patch-defun markdown-cycle (&optional arg)
  "Visibility cycling for Markdown mode.
This function is called with a `\\[universal-argument]' or if ARG is t, perform
global visibility cycling.  If the point is at an atx-style header, cycle
visibility of the corresponding subtree.  Otherwise, indent the current line
 or insert a tab, as appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond

   ;; Global cycling
   (arg
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 2))
      (outline-hide-sublevels 1)
      (message "CONTENTS")
      (setq markdown-cycle-global-status 3)
      (markdown-outline-fix-visibility))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 3))
      (outline-show-all)
      (message "SHOW ALL")
      (setq markdown-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (outline-hide-body)
      (message "OVERVIEW")
      (setq markdown-cycle-global-status 2)
      (markdown-outline-fix-visibility))))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (markdown-on-heading-p))
    (markdown-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (markdown-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (markdown-end-of-heading)   (setq eoh (point))
        (markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq markdown-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        (markdown-show-entry)
        (outline-show-children)
        (message "CHILDREN")
        (setq markdown-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq markdown-cycle-subtree-status 'children))
        (outline-show-subtree)
        (message "SUBTREE")
        (setq markdown-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (outline-hide-subtree)
        (message "FOLDED")
        (setq markdown-cycle-subtree-status 'folded)))))

   ;; In a table, move forward by one cell
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-forward-cell))

   ;; Otherwise, indent as appropriate
   (t
    (el-patch-swap
      (indent-for-tab-command)
      (if (or (bolp) (looking-back "^[ \t]*" 16))
          (indent-for-tab-command)
        (completion-at-point))))))
