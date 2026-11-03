;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/workspaces/test/autoload-workspaces.el

(require 'cl-lib)
(require 'persp-mode)
(require 'tab-bar)
(require 'zenit-test)
(zenit-require 'zenit-lib 'modules)
(zenit-modules-initialize)
(zenit-load (zenit-module-locate-path :ui 'workspaces "autoload/workspaces.el"))

(zenit-deftest +workspace-current
  ()
  (should (eq (indirect-function 'persp-get-current)
              (indirect-function '+workspace-current))))

(zenit-deftest +workspace-current-name
  ()
  (let ((persp-nil-persp (persp--make :name "nil-workspace")))
    (cl-letf (((symbol-function '+workspace-current) (lambda () ,persp)))
      (should (equal ,name (+workspace-current-name)))))
  (persp name)
  (persp--make :name "test") "test"
  nil "nil-workspace")

(zenit-deftest +workspace-switch
  ()
  (let* ((frame (selected-frame))
         (local-workspaces '("main" ".emacs.d"))
         ;; Put the other frame first, as happens when it is created later.
         (+workspaces-frames-alist
          `((other-frame "main" "remote") (,frame ,@local-workspaces)))
         (+workspace--last "previous")
         (current-name ".emacs.d")
         switched focused updated)
    (cl-letf (((symbol-function '+workspace-exists-p)
               (lambda (name)
                 (member name '("main" ".emacs.d" "remote" "unassigned"))))
              ((symbol-function '+workspace-current-name)
               (lambda () current-name))
              ((symbol-function 'frame-parameter)
               (lambda (target parameter)
                 (should (eq frame target))
                 (should (eq 'workspaces parameter))
                 local-workspaces))
              ((symbol-function 'persp-frame-switch)
               (lambda (name target)
                 (setq switched (list name target)
                       current-name name)))
              ((symbol-function 'select-frame-set-input-focus)
               (lambda (target &rest _) (setq focused target)))
              ((symbol-function '+workspaces--update-tab-bar)
               (lambda () (setq updated t))))
      ,test))
  (test)

  :doc "Shared workspaces switch in the current frame regardless of alist order"
  (progn
    (should (+workspace-switch "main"))
    (should (equal (list "main" frame) switched))
    (should (eq frame focused))
    (should (equal ".emacs.d" +workspace--last))
    (should updated))

  :doc "Workspaces belonging only to another frame still focus that frame"
  (progn
    (should (+workspace-switch "remote"))
    (should (equal '("remote" other-frame) switched))
    (should (eq 'other-frame focused))
    (should (equal ".emacs.d" +workspace--last))
    (should updated))

  :doc "Unassociated workspaces fall back to the current frame"
  (progn
    (should (+workspace-switch "unassigned"))
    (should (equal (list "unassigned" frame) switched))
    (should (eq frame focused))
    (should updated))

  :doc "Switching to the active workspace does not focus another frame"
  (progn
    (setq current-name "main")
    (should (+workspace-switch "main"))
    (should-not switched)
    (should-not focused)
    (should-not updated)
    (should (equal "previous" +workspace--last))))

(zenit-deftest +workspaces-ignore-child-frame-h
  ()
  (let* ((frame (selected-frame))
         (persp (persp--make :name "test" :window-conf 'original-layout))
         (parent frame)
         ignored
         (captured nil)
         (persp-save-restore-window-conf-filter-functions
          (cons #'+workspaces-ignore-child-frame-h
                persp-save-restore-window-conf-filter-functions)))
    (cl-letf (((symbol-function 'frame-parent) (lambda (_) parent))
              ((symbol-function 'set-frame-parameter)
               (lambda (target parameter value)
                 (should (eq frame target))
                 (should (eq 'persp-ignore-wconf parameter))
                 (setq ignored value)))
              ((symbol-function 'persp-of-frame) (lambda (_) persp))
              ((symbol-function 'persp-window-state-get)
               (lambda (&rest _) (setq captured t) 'new-layout))
              ((symbol-function 'persp-window-state-put)
               (lambda (&rest _) (setq captured t))))
      ,test))
  (test)

  :doc "Child-frame saves cannot overwrite the workspace window configuration"
  (progn
    (persp-frame-save-state frame)
    (should-not captured)
    (should ignored)
    (should (eq 'original-layout (persp-window-conf persp))))

  :doc "Child frames do not restore a workspace window configuration"
  (progn
    (persp-restore-window-conf frame persp)
    (should-not captured))

  :doc "Ordinary frames still save the workspace window configuration"
  (progn
    (setq parent nil)
    (persp-frame-save-state frame)
    (should captured)
    (should-not ignored)
    (should (eq 'new-layout (persp-window-conf persp))))

  :doc "Ordinary frames still restore the workspace window configuration"
  (progn
    (setq parent nil)
    (persp-restore-window-conf frame persp)
    (should captured))

  :doc "Existing child frames are excluded when choosing a frame to save"
  (progn
    (+workspaces-ignore-child-frame-h frame)
    (cl-letf (((symbol-function 'persp-frames-with-persp)
               (lambda (_) (list frame 'normal-frame)))
              ((symbol-function 'frame-parameter)
               (lambda (target parameter)
                 (and (eq frame target)
                      (eq 'persp-ignore-wconf parameter)
                      ignored))))
      (should (eq 'normal-frame (persp-find-other-frame-with-persp persp nil t))))))

(zenit-deftest +workspaces-save-tab-bar-data-h
  ()
  (let* ((frame (selected-frame))
         (persp (persp--make :name "test"))
         (parent frame)
         (tabs '((current-tab (name . "current"))))
         (original-tabs '((current-tab (name . "original"))))
         (tab-bar-closed-tabs '(current-closed))
         captured)
    (persp-set-parameter 'tab-bar-tabs (list (cons frame original-tabs)) persp)
    (persp-set-parameter 'tab-bar-closed-tabs 'original-closed persp)
    (cl-letf (((symbol-function 'frame-parent) (lambda (_) parent))
              ((symbol-function 'tab-bar-tabs)
               (lambda (_) (setq captured t) tabs)))
      ,test))
  (test)

  :doc "Child tab data cannot replace saved workspace tabs or closed tabs"
  (progn
    (+workspaces-save-tab-bar-data-h 'frame frame persp)
    (should-not captured)
    (should (equal (list (cons frame original-tabs))
                   (persp-parameter 'tab-bar-tabs persp)))
    (should (eq 'original-closed
                (persp-parameter 'tab-bar-closed-tabs persp))))

  :doc "Ordinary frames still save their tabs and closed tabs"
  (progn
    (setq parent nil)
    (+workspaces-save-tab-bar-data-h 'frame frame persp)
    (should captured)
    (should (equal (list (cons frame tabs))
                   (persp-parameter 'tab-bar-tabs persp)))
    (should (equal (list (cons frame '(current-closed)))
                   (persp-parameter 'tab-bar-closed-tabs persp)))))

(zenit-deftest +workspaces-load-tab-bar-data-h
  ()
  (let* ((frame (selected-frame))
         (persp (persp--make :name "test"))
         (parent frame)
         (tabs (list (list 'current-tab '(name . "saved"))))
         (tab-bar-closed-tabs '(original-closed))
         restored updated)
    (persp-set-parameter 'tab-bar-tabs (list (cons frame tabs)) persp)
    (persp-set-parameter 'tab-bar-closed-tabs
                         (list (cons frame '(saved-closed))) persp)
    (cl-letf (((symbol-function 'frame-parent) (lambda (_) parent))
              ((symbol-function '+workspace-current-name) (lambda () "test"))
              ((symbol-function 'tab-bar-tabs-set)
               (lambda (value target)
                 (should (eq frame target))
                 (setq restored value)))
              ((symbol-function 'tab-bar--update-tab-bar-lines)
               (lambda (&rest _) (setq updated t))))
      ,test))
  (test)

  :doc "Child activation does not restore tabs or overwrite closed-tab history"
  (progn
    (+workspaces-load-tab-bar-data-h 'frame frame persp)
    (should-not restored)
    (should-not updated)
    (should (equal '(original-closed) tab-bar-closed-tabs))
    (should-not (alist-get 'group (cdar tabs))))

  :doc "Ordinary frames still restore and group tabs and closed-tab history"
  (progn
    (setq parent nil)
    (+workspaces-load-tab-bar-data-h 'frame frame persp)
    (should (eq tabs restored))
    (should updated)
    (should (equal '(saved-closed) tab-bar-closed-tabs))
    (should (equal "test" (alist-get 'group (cdar tabs))))))

(zenit-deftest +workspaces--tab-bar-tabs-for-frame
  ()
  (let ((tabs '((child (current-tab (name . "minibuffer")))
                (dead (current-tab (name . "dead")))
                (normal (current-tab (name . "normal"))))))
    (cl-letf (((symbol-function 'framep)
               (lambda (frame) (memq frame '(child dead normal requested))))
              ((symbol-function 'frame-live-p) (lambda (frame) (not (eq 'dead frame))))
              ((symbol-function 'frame-parent) (lambda (frame) (eq 'child frame))))
      ,test))
  (test)

  :doc "Fallback ignores saved child-frame and dead-frame entries"
  (should (equal '((current-tab (name . "normal")))
                 (+workspaces--tab-bar-tabs-for-frame tabs 'requested)))

  :doc "A frame's own tabs take precedence over fallback entries"
  (should (eq (cdr (assq 'normal tabs))
              (+workspaces--tab-bar-tabs-for-frame tabs 'normal)))

  :doc "Readable tab lists remain compatible with saved sessions"
  (let ((readable '((current-tab (name . "saved")))))
    (should (eq readable
                (+workspaces--tab-bar-tabs-for-frame readable 'requested)))))
