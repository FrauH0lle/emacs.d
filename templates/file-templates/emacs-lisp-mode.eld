emacs-lisp-mode

;; Templates for emacs-lisp files used within this Emacs configuration
(__
 ";;; " (or (buffer-file-name) (buffer-name)) " -- " (p "DESCRIPTION") " -*- lexical-binding: t -*-" n n
 q n n
 "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
 ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)

(__zenit-control
 ";; -*- lexical-binding: t; no-byte-compile: t; -*-" n
 ";; " (+file-templates-get-short-path) n n)

(__zenit-module ";; " (+file-templates-get-short-path) " -*- lexical-binding: t; -*-" n n)

(__zenit-packages
 ";; -*- lexical-binding: t; no-byte-compile: t; -*-" n
 ";; " (+file-templates-get-short-path) n n)

(__zenit-test
 ";; -*- lexical-binding: t; no-byte-compile: t; -*-" n
 ";; " (+file-templates-get-short-path) n n)
