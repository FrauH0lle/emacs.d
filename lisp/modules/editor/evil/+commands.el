;; editor/evil/+commands.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'evil-ex))

;; Editing
(evil-ex-define-cmd "R[ead]"       #'+evil:read)
(evil-ex-define-cmd "al[ign]"      #'+evil:align)
(evil-ex-define-cmd "ral[ign]"     #'+evil:align-right)
(evil-ex-define-cmd "mc"           #'+multiple-cursors:evil-mc)
(evil-ex-define-cmd "iedit"        #'evil-multiedit-ex-match)
(evil-ex-define-cmd "na[rrow]"     #'+evil:narrow-buffer)
(evil-ex-define-cmd "retab"        #'+evil:retab)
(evil-ex-define-cmd "rev[erse]"    #'+evil:reverse-lines)
(evil-ex-define-cmd "l[ine]diff"   #'evil-quick-diff)

;; External
(evil-ex-define-cmd "dash"      #'+lookup:dash)
(evil-ex-define-cmd "repl"      #'+eval:repl)

;; Git
(evil-ex-define-cmd "gbrowse"   #'+vc:git-browse)
(evil-ex-define-cmd "git"       #'magit-status)
(evil-ex-define-cmd "gstage"    #'magit-stage)
(evil-ex-define-cmd "gunstage"  #'magit-unstage)
(evil-ex-define-cmd "gblame"    #'magit-blame)
(evil-ex-define-cmd "grevert"   #'+vc-gutter/revert-hunk)

;; Buffers
(evil-ex-define-cmd "k[ill]"    #'kill-current-buffer)
(evil-ex-define-cmd "k[ill]all" #'+evil:kill-all-buffers)
(evil-ex-define-cmd "k[ill]m"   #'+evil:kill-matching-buffers)
(evil-ex-define-cmd "k[ill]o"   #'+emacs/kill-other-buffers)
(evil-ex-define-cmd "k[ill]p"   #'+emacs/kill-project-buffers)
(evil-ex-define-cmd "l[ast]"    #'+popup/restore)
(evil-ex-define-cmd "messages"  #'view-echo-area-messages)
(evil-ex-define-cmd "pop[up]"   #'+popup/buffer)
(evil-ex-define-cmd "scr[atch]" #'+evil:open-scratch-buffer)

;; Project navigation
(evil-ex-define-cmd "a"           #'find-sibling-file)
(evil-ex-define-cmd "cd"          #'+evil:cd)
(evil-ex-define-cmd "pwd"         #'+evil:pwd)

;; Project navigation
(evil-ex-define-cmd "a"         #'projectile-find-other-file)
(evil-ex-define-cmd "cd"        #'+evil:cd)
(evil-ex-define-cmd "pwd"       #'+evil:pwd)

(cond ((modulep! :completion vertico)
       (evil-ex-define-cmd "pg[rep]"   #'+vertico:project-search)
       (evil-ex-define-cmd "pg[grep]d" #'+vertico:project-search-from-cwd)))

;; Project tools
(evil-ex-define-cmd "com[pile]"   #'+evil:compile)
(evil-ex-define-cmd "make"        #'+evil:make)
(evil-ex-define-cmd "debug"       #'+debugger/start)
(evil-ex-define-cmd "er[rors]"    #'flycheck-list-errors)

;; Files
(evil-ex-define-cmd "cp"        #'+evil:copy-this-file)
(evil-ex-define-cmd "mv"        #'+evil:move-this-file)
(evil-ex-define-cmd "rm"        #'+evil:delete-this-file)

;; Session
(evil-ex-define-cmd "sclear"      #'+workspace/kill-session)
(evil-ex-define-cmd "sl[oad]"     #'+emacs/quickload-session)
(evil-ex-define-cmd "ss[ave]"     #'+emacs/quicksave-session)
(evil-ex-define-cmd "tabc[lose]"  #'+workspace:delete)
(evil-ex-define-cmd "tabclear"    #'+emacs/kill-all-buffers)
(evil-ex-define-cmd "tabl[ast]"   #'+workspace/switch-to-last)
(evil-ex-define-cmd "tabload"     #'+workspace:load)
(evil-ex-define-cmd "tabn[ew]"    #'+workspace:new)
(evil-ex-define-cmd "tabnext"     #'+workspace:switch-next)
(evil-ex-define-cmd "tabprev"     #'+workspace:switch-previous)
(evil-ex-define-cmd "tabr[ename]" #'+workspace:rename)
(evil-ex-define-cmd "tabs"        #'+workspace/display)
(evil-ex-define-cmd "tabsave"     #'+workspace:save)

;; ibuffer
(static-when (modulep! :emacs ibuffer)
  (evil-ex-define-cmd "buffers" #'ibuffer))
