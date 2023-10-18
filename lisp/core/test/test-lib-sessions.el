;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-sessions.el

(describe "core/lib/sessions"

  (load! "lib/sessions" zenit-core-dir)

  (before-each
    (setq persp-auto-save-fname "persp-auto-save-fname"
          persp-save-dir temporary-file-directory
          persp-mode nil))

  (describe "zenit-session-file"
    (it "returns the default session file path for persp-mode"
      (spy-on 'require :and-return-value t)
      (spy-on 'expand-file-name :and-call-through)
      (expect (zenit-session-file)
              :to-equal (expand-file-name persp-auto-save-fname persp-save-dir))
      (expect 'require
              :to-have-been-called-with 'persp-mode nil t))

    (it "returns the default session file path for desktop"
      (spy-on 'require :and-call-fake (lambda (feature &optional _filename _noerror)
                                        (when (eq feature 'desktop)
                                          t)))
      (spy-on 'desktop-full-file-name :and-return-value (concat temporary-file-directory "desktop.el"))
      (expect (zenit-session-file)
              :to-equal (desktop-full-file-name))
      (expect 'require
              :to-have-been-called-with 'desktop nil t))

    (it "returns a custom session file path"
      (spy-on 'require :and-return-value t)
      (spy-on 'expand-file-name :and-call-through)
      (let ((custom-name "custom-session.el"))
        (expect (zenit-session-file custom-name)
                :to-equal (expand-file-name custom-name persp-save-dir))))

    (it "signals an error when no session backend is available"
      (spy-on 'require :and-return-value nil)
      (expect (zenit-session-file) :to-throw 'error)))

  (describe "zenit-save-session"
    (it "saves the current session state using persp-mode"
      (spy-on 'require :and-return-value t)
      (spy-on 'persp-mode)
      (spy-on 'persp-save-state-to-file)
      (let ((file (concat temporary-file-directory "persp-session.el")))
        (zenit-save-session file)
        (expect 'persp-mode :to-have-been-called-with +1)
        (expect 'persp-save-state-to-file :to-have-been-called-with file)))

    (it "saves the current session state using desktop"
      (spy-on 'require :and-call-fake (lambda (feature &optional _filename _noerror)
                                        (when (or (eq feature 'frameset)
                                                  (eq feature 'restart-emacs))
                                          t)))
      (spy-on 'desktop-save)
      (let ((file (concat temporary-file-directory "desktop-session.el"))
            (desktop-dirname temporary-file-directory))
        (zenit-save-session file)
        (expect 'desktop-save :to-have-been-called-with desktop-dirname t)))

    (it "signals an error when no session backend is available"
      (spy-on 'require :and-return-value nil)
      (expect (zenit-save-session) :to-throw 'error)))

  (describe "zenit-load-session"
    (it "loads the session state using persp-mode"
      (spy-on 'message)
      (spy-on 'require :and-return-value t)
      (spy-on 'persp-mode)
      (spy-on 'persp-list-persp-names-in-file)
      (spy-on 'persp-load-state-from-file)
      (spy-on 'persp-kill)
      (setq *persp-hash* (make-hash-table))

      (let ((file (concat temporary-file-directory "persp-session.el")))
        (zenit-load-session file)
        (expect 'persp-mode :to-have-been-called-with +1)
        (expect 'persp-load-state-from-file :to-have-been-called-with file)))

    (it "loads the session state using desktop"
      (spy-on 'message)
      (spy-on 'require :and-call-fake (lambda (feature &optional _filename _noerror)
                                        (when (or (eq feature 'frameset)
                                                  (eq feature 'restart-emacs))
                                          t)))
      (spy-on 'restart-emacs--restore-frames-using-desktop)
      (let ((file (concat temporary-file-directory "desktop-session.el")))
        (zenit-load-session file)
        (expect 'restart-emacs--restore-frames-using-desktop :to-have-been-called-with file)))

    (it "signals an error when no session backend is available"
      (spy-on 'require :and-return-value nil)
      (expect (zenit-load-session) :to-throw 'error)))

  (describe "zenit/quickload-session"
    (it "calls zenit-load-session with no file argument"
      (spy-on 'message)
      (spy-on 'yes-or-no-p :and-return-value t)
      (spy-on 'zenit-load-session)
      (zenit/quickload-session)
      (expect 'zenit-load-session :to-have-been-called)))

  (describe "zenit/quicksave-session"
    (it "calls zenit-save-session with no file argument"
      (spy-on 'message)
      (spy-on 'zenit-save-session)
      (zenit/quicksave-session)
      (expect 'zenit-save-session :to-have-been-called)))

  (describe "zenit/load-session"
    (it "calls zenit-load-session with a file argument"
      (spy-on 'message)
      (spy-on 'zenit-load-session)
      (let ((file (concat temporary-file-directory "session-file.el")))
        (zenit/load-session file)
        (expect 'zenit-load-session :to-have-been-called-with file))))

  (describe "zenit/save-session"
    (it "calls zenit-save-session with a file argument"
      (spy-on 'message)
      (spy-on 'zenit-save-session)
      (let ((file (concat temporary-file-directory "session-file.el")))
        (zenit/save-session file)
        (expect 'zenit-save-session :to-have-been-called-with file))))

  (describe "zenit/restart"
    (it "calls restart-emacs"
      (spy-on 'restart-emacs)
      (zenit/restart)
      (expect 'restart-emacs :to-have-been-called)))

  (describe "zenit/new-emacs"
    (it "calls restart-emacs-start-new-emacs"
      (spy-on 'restart-emacs-start-new-emacs)
      (zenit/new-emacs)
      (expect 'restart-emacs-start-new-emacs :to-have-been-called)))

  (describe "zenit/restart-and-restore"
    (it "calls zenit/quicksave-session and restart-emacs"
      (spy-on 'message)
      (spy-on 'zenit/quicksave-session)
      (spy-on 'restart-emacs)
      (zenit/restart-and-restore)
      (expect 'zenit/quicksave-session :to-have-been-called)
      (expect 'restart-emacs :to-have-been-called))))
