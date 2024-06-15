prog-mode

;; Tags
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO " (i today) ": ")
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME " (i today) ": ")
(review (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "REVIEW " (i today) ": ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK " (i today) ": ")
(patch (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "PATCH " (i today) ": ")
(deprecated (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "DEPRECATED " (i today) ": ")
(note (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "NOTE " (i today) ": ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG " (i today) ": ")
(xxx (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "XXX " (i today) ": ")
