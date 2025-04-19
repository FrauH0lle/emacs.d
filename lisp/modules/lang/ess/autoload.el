;; lang/ess/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ess/goto-end-of-prompt ()
  "Move cursor to the prompt when switching to insert mode (if point
isn't already there)."
  (interactive)
  (goto-char (point-max))
  (evil-append 1))

;;;###autoload
(defun +ess-R-remote (&optional remote-host session cmds)
  "Create and connect to a REMOTE-HOST's R session.

REMOTE-HOST, SESSION and CMDS are read interactively or by
setting the variables `+ess-R-remote-host',

`+ess-R-remote-session' and `+ess-R-remote-cmds'. CMDS is a list
of strings and are run before the R process is created. It is a
good idea to end them with \"&&\"."
  (interactive)
  (let ((remote-p (file-remote-p default-directory)))
    (when remote-p
      (display-warning 'ESS (concat "Display of graphical elements will "
                                    "not work as we are running directly "
                                    "on a remote host.")))
    (let* ((remote-p (file-remote-p default-directory))
           (remote-host (or remote-host
                            +ess-R-remote-host
                            (file-remote-p default-directory 'host)
                            (read-from-minibuffer "R remote host: "
                                                  "your.remote.server")))
           (session (or session
                        +ess-R-remote-session
                        (read-from-minibuffer "R remote session: "
                                              "R-session-name")))
           (cmds (or cmds
                     +ess-R-remote-cmds
                     (+emacs-enlist
                      (split-string
                       (read-from-minibuffer "CMDs to start the R session: ")))))
           (comint-process-echoes t)
           (buf (apply #'make-comint (concat "R:" remote-host "-" session)
                       ;; If we are on a remote and CMDS is nil, just try to
                       ;; launch R
                       (cond ((and remote-p (not cmds)) inferior-R-program-name)
                             ;; If we are on a remote and CMDS is non-nil, use sh
                             ;; -c start R via CMDS
                             ((and remote-p cmds) "sh")
                             ;; Else we are not on a remote and need to use SSH to
                             ;; connect
                             (t "ssh"))
                       ;; No startfile
                       nil
                       ;; Construct switches
                       (if remote-p
                           ;; If we are on a remote server and CMDS is nil,
                           ;; appends normal R switches and be done
                           (cond ((and remote-p (not cmds)) (list "--no-readline" inferior-R-args))
                                 ;; If we are on a remote server and CMDS is non-nil,
                                 ;; use them to start R session via "sh -c CMDS"
                                 (cmds (append (list "-c")
                                               (list
                                                (string-join
                                                 (append cmds
                                                         (list inferior-R-program-name)
                                                         '("--no-readline")
                                                         (list inferior-R-args)) " ")))))
                         ;; Else we want to connect via SSH
                         (append (list "-Y" "-C" "-t")
                                 (list remote-host)
                                 ;; If CMDS in non-nil, we use them here to start
                                 ;; R
                                 (if cmds
                                     (append cmds
                                             (list inferior-R-program-name)
                                             (list "--no-readline")
                                             (list inferior-R-args))
                                   ;; Else use normal start arguments
                                   (list inferior-R-program-name)
                                   (list "--no-readline")
                                   (list inferior-R-args)))))))
      (with-current-buffer buf
        (ess-remote (concat "R:" remote-host "-" session) "R"))
      (pop-to-buffer buf))))
