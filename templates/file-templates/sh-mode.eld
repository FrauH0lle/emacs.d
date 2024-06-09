sh-base-mode

(__ "#!/usr/bin/env " (if (equal (file-name-extension buffer-file-name) "zsh") "zsh" "bash") n n
    q)
