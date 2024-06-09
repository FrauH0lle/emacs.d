org-mode

(__ "#+title: "
    (p (string-join
        (mapcar #'capitalize
                ;; Replace -,_... with space
                (split-string
                 (let (case-fold-search)
                   ;; Seperating lower from upper: hello|World
                   (replace-regexp-in-string
                    "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
                    ;; Separating upper from (upper and lower): HTTP|Server
                    (replace-regexp-in-string "\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)"
                                              "\\1 \\2" (file-name-base buffer-file-name))))
                 "[^[:word:]0-9]+"
                 )) " " ))
    n n
    q)

(__zenit-module-readme
 "#+title: " (+file-templates-module-for-path) n
 "#+created: " (format-time-string "%Y-%m-%d") n
 n
 "* Description :unfold:" n
 (p "A short description of what this module does.") n
 n
 "If there are any interesting things about this module, add it here as well." n
 "For example funky hacks, tweaks, ..." n
 n
 "** Module flags" n
 "Flags should be in alphanumerical order." n
 "- +bar ::" n
 "  A short description of what this flag does and what it might need when enabled." n
 n
 "If this module has no flags, then ..." n
 "/None/"n
 n
 "** Packages" n
  "Packages should be a include a link to the homepage or repository." n
  "- [[https://protesilaos.com/emacs/pulsar][Pulsar]]" n
  n
  "If this module installs no packages, then ..." n
  "/None/"n
  n
  "* Installation" n
  "Add any external requirements etc to this section." n
  n
  "* Configuration" n
  "List user facing configuration variables like so:" n
  "#+begin_src elisp" n
  "(defvar +nav-flash-exclude-commands" n
  "  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point" n
  "    org-find-file org-find-file-at-mouse)" n
  "\"A list of commands that should not trigger nav-flash.\")" n
  "#+end_src" n
  n
  "Of course you can write 1 or 2 sentences as well ..." n)
