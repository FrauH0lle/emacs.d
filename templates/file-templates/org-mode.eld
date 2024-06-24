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
 "
If there are any interesting things about this module, add it here as well.
For example funky hacks, tweaks, ...

** Module flags
Flags should be in alphanumerical order.
- +bar ::
  A short description of what this flag does and what it might need when enabled.

If this module has no flags, then ...
/None/

** Packages
Packages should be a include a link to the homepage or repository.
- [[https://protesilaos.com/emacs/pulsar][Pulsar]]

If this module installs no packages, then ...
/None/

* Installation
Add any external requirements etc to this section.

* Configuration
List user facing configuration variables like so:
#+begin_src elisp
(defvar +nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
    org-find-file org-find-file-at-mouse)
\"A list of commands that should not trigger nav-flash.\")
#+end_src

Of course you can write 1 or 2 sentences as well ...")
