;; tools/llm/patches.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'el-patch)
  (require 'gptel-integrations))

(el-patch-defun gptel-mcp-connect (&optional servers server-callback interactive (el-patch-add syncp))
  "Add gptel tools from MCP servers using the mcp package.

MCP servers are started if required.  SERVERS is a list of server
names (strings) to connect to.  If nil, all known servers are
considered.

If INTERACTIVE is non-nil (or called interactively), guide the user
through setting up mcp, and query for servers to retrieve tools from.

Call SERVER-CALLBACK after starting MCP servers."
  (interactive (list nil nil t))
  (if (locate-library "mcp-hub")
      (unless (require 'mcp-hub nil t)
        (user-error "Could not load `mcp-hub'!  Please install\
 or configure the mcp package"))
    (user-error "Could not find mcp!  Please install or configure the mcp package"))
  (if (null mcp-hub-servers)
      (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
    (setq servers
          (if servers
              (mapcar (lambda (s) (assoc s mcp-hub-servers)) servers)
            mcp-hub-servers))
    (let ((unregistered-servers ;Available servers minus servers already registered with gptel
           (cl-loop for server in servers
                    with registered-names =
                    (cl-loop for (cat . _tools) in gptel--known-tools
                             if (string-prefix-p "mcp-" cat)
                             collect (substring cat 4))
                    unless (member (car server) registered-names)
                    collect server)))
      (if unregistered-servers
          (let* ((servers
                  (if interactive
                      (let ((picks
                             (completing-read-multiple
                              "Add tools from MCP servers (separate with \",\"): "
                              (cons '("ALL") unregistered-servers) nil t)))
                        (if (member "ALL" picks)
                            unregistered-servers
                          (mapcar (lambda (s) (assoc s mcp-hub-servers)) picks)))
                    unregistered-servers))
                 (server-active-p
                  (lambda (server)
                    (when-let* ((server (gethash (car server) mcp-server-connections)))
                      (equal (mcp--status server) 'connected))))
                 (inactive-servers (cl-remove-if server-active-p servers))
                 (add-all-tools
                  (lambda (&optional server-names)
                    "Register and add tools from servers.  Report failures."
                    (let ((tools (gptel-mcp--get-tools server-names))
                          (now-active (cl-remove-if-not server-active-p mcp-hub-servers)))
                      (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
                      (gptel-mcp--activate-tools tools)
                      (if-let* ((failed (cl-set-difference inactive-servers now-active
                                                           :test #'equal)))
                          (progn
                            (message "Inactive-before: %S, Now-Active: %S" inactive-servers now-active)
                            (message (substitute-command-keys
                                      "%d/%d server%s failed to start: %s.  Run \\[mcp-hub] to investigate.")
                                     (length failed) (length inactive-servers)
                                     (if (= (length failed) 1) "" "s")
                                     (mapconcat #'car failed ", ")))
                        (let ((added (or server-names (mapcar #'car now-active))))
                          (message "Added %d tools from %d MCP server%s: %s"
                                   (length tools) (length added)
                                   (if (= (length added) 1) "" "s")
                                   (mapconcat #'identity added ", "))))
                      (when (functionp server-callback) (funcall server-callback))))))

            (if inactive-servers        ;start servers
                (mcp-hub-start-all-server
                 add-all-tools (mapcar #'car inactive-servers) (el-patch-add syncp))
              (funcall add-all-tools (mapcar #'car servers))))
        (message "All MCP tools are already available to gptel!")
        (when (functionp server-callback) (funcall server-callback))))))

;; (defun gptel-mcp-connect (&optional servers server-callback interactive syncp)
;;   "Add gptel tools from MCP servers using the mcp package.

;; MCP servers are started if required.  SERVERS is a list of server
;; names (strings) to connect to.  If nil, all known servers are
;; considered.

;; If INTERACTIVE is non-nil (or called interactively), guide the user
;; through setting up mcp, and query for servers to retrieve tools from.

;; Call SERVER-CALLBACK after starting MCP servers."
;;   (interactive (list nil nil t))
;;   (if (locate-library "mcp-hub")
;;       (unless (require 'mcp-hub nil t)
;;         (user-error "Could not load `mcp-hub'!  Please install\
;;  or configure the mcp package"))
;;     (user-error "Could not find mcp!  Please install or configure the mcp package"))
;;   (if (null mcp-hub-servers)
;;       (user-error "No MCP servers available!  Please configure `mcp-hub-servers'")
;;     (setq servers
;;           (if servers
;;               (mapcar (lambda (s) (assoc s mcp-hub-servers)) servers)
;;             mcp-hub-servers))
;;     (let ((unregistered-servers ;Available servers minus servers already registered with gptel
;;            (cl-loop for server in servers
;;                     with registered-names =
;;                     (cl-loop for (cat . _tools) in gptel--known-tools
;;                              if (string-prefix-p "mcp-" cat)
;;                              collect (substring cat 4))
;;                     unless (member (car server) registered-names)
;;                     collect server)))
;;       (if unregistered-servers
;;           (let* ((servers
;;                   (if interactive
;;                       (let ((picks
;;                              (completing-read-multiple
;;                               "Add tools from MCP servers (separate with \",\"): "
;;                               (cons '("ALL") unregistered-servers) nil t)))
;;                         (if (member "ALL" picks)
;;                             unregistered-servers
;;                           (mapcar (lambda (s) (assoc s mcp-hub-servers)) picks)))
;;                     unregistered-servers))
;;                  (server-active-p
;;                   (lambda (server)
;;                     (when-let* ((server (gethash (car server) mcp-server-connections)))
;;                       (equal (mcp--status server) 'connected))))
;;                  (inactive-servers (cl-remove-if server-active-p servers))
;;                  (add-all-tools
;;                   (lambda (&optional server-names)
;;                     "Register and add tools from servers.  Report failures."
;;                     (let ((tools (gptel-mcp--get-tools server-names))
;;                           (now-active (cl-remove-if-not server-active-p mcp-hub-servers)))
;;                       (mapc (lambda (tool) (apply #'gptel-make-tool tool)) tools)
;;                       (gptel-mcp--activate-tools tools)
;;                       (if-let* ((failed (cl-set-difference inactive-servers now-active
;;                                                            :test #'equal)))
;;                           (progn
;;                             (message "Inactive-before: %S, Now-Active: %S" inactive-servers now-active)
;;                             (message (substitute-command-keys
;;                                       "%d/%d server%s failed to start: %s.  Run \\[mcp-hub] to investigate.")
;;                                      (length failed) (length inactive-servers)
;;                                      (if (= (length failed) 1) "" "s")
;;                                      (mapconcat #'car failed ", ")))
;;                         (let ((added (or server-names (mapcar #'car now-active))))
;;                           (message "Added %d tools from %d MCP server%s: %s"
;;                                    (length tools) (length added)
;;                                    (if (= (length added) 1) "" "s")
;;                                    (mapconcat #'identity added ", "))))
;;                       (when (functionp server-callback) (funcall server-callback))))))

;;             (if inactive-servers        ;start servers
;;                 (mcp-hub-start-all-server
;;                  add-all-tools (mapcar #'car inactive-servers) syncp)
;;               (funcall add-all-tools (mapcar #'car servers))))
;;         (message "All MCP tools are already available to gptel!")
;;         (when (functionp server-callback) (funcall server-callback))))))
