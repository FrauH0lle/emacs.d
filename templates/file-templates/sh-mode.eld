sh-base-mode

(__ (let ((name (or (buffer-file-name) (buffer-name))))
      (cond ((string-suffix-p ".bash" name)
             "#!/usr/bin/env bash")
            ((string-suffix-p ".zsh" name)
             "#!/usr/bin/env zsh")
            (t
             (let ((shell (completing-read "Shell: " '("Bash" "POSIX sh" "Zsh"))))
               (cond ((member shell '("bash" "zsh"))
                      (concat "#!/usr/bin/env " shell))
                     (t "#!/bin/sh"))))))
    n n)
