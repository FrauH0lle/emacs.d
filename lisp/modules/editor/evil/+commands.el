;; editor/evil/+commands.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'evil-ex))


;;
;;; Custom commands

(evil-ex-define-cmd "R[ead]"       #'+evil:read)
(evil-ex-define-cmd "al[ign]"      #'+evil:align)
(evil-ex-define-cmd "ral[ign]"     #'+evil:align-right)
(static-when (modulep! :editor multiple-cursors)
  (evil-ex-define-cmd "mc"           #'+multiple-cursors:evil-mc))
(evil-ex-define-cmd "iedit"        #'evil-multiedit-ex-match)
(evil-ex-define-cmd "na[rrow]"     #'+evil:narrow-buffer)
(evil-ex-define-cmd "retab"        #'+evil:retab)
(evil-ex-define-cmd "rev[erse]"    #'+evil:reverse-lines)
(evil-ex-define-cmd "l[ine]diff"   #'evil-quick-diff)


;;
;;; External resources

(static-when (modulep! :tools lookup)
  (evil-ex-define-cmd "lo[okup]"  #'+lookup:online)
  (evil-ex-define-cmd "dash"      #'+lookup:dash))
(static-when (modulep! :tools eval)
  (evil-ex-define-cmd "repl"      #'+eval:repl))
(evil-ex-define-cmd "h[elp]"    #'+evil:help)
(static-when (modulep! :emacs eshell)
  (evil-ex-define-cmd "sh[ell]"     #'+eshell:run))
(evil-ex-define-cmd "pad"         #'+evil:open-scratch-buffer)


;;
;;; Git

(static-when (modulep! :emacs vc)
  (evil-ex-define-cmd "gbrowse"     #'+vc/browse-at-remote)) ; show file/region in github/gitlab
(static-when (modulep! :tools magit +forge)
  (evil-ex-define-cmd "gissues"     #'forge-browse-issues))  ; show github issues
(static-when (modulep! :tools magit)
  (evil-ex-define-cmd "git"       #'magit-status)
  (evil-ex-define-cmd "gstage"    #'magit-stage)
  (evil-ex-define-cmd "gunstage"  #'magit-unstage)
  (evil-ex-define-cmd "gblame"    #'magit-blame))
(static-when (modulep! :ui vc-gutter)
  (evil-ex-define-cmd "grevert"   #'+vc-gutter/revert-hunk))


;;
;;; Dealing with buffers

(evil-ex-define-cmd "k[ill]"    #'kill-current-buffer)
(evil-ex-define-cmd "k[ill]all" #'+evil:kill-all-buffers)
(evil-ex-define-cmd "k[ill]m"   #'+evil:kill-matching-buffers)
(evil-ex-define-cmd "k[ill]o"   #'zenit/kill-other-buffers)
(evil-ex-define-cmd "k[ill]p"   #'zenit/kill-project-buffers)
(static-when (modulep! :ui popup)
  (evil-ex-define-cmd "l[ast]"    #'+popup/restore))
(evil-ex-define-cmd "messages"  #'view-echo-area-messages)
(static-when (modulep! :ui popup)
  (evil-ex-define-cmd "pop[up]"   #'+popup/buffer))


;;
;;; Project navigation

(evil-ex-define-cmd "a"         #'projectile-find-other-file)
(evil-ex-define-cmd "cd"        #'+evil:cd)
(evil-ex-define-cmd "pwd"       #'+evil:pwd)

(static-when (modulep! :completion vertico)
  (evil-ex-define-cmd "pg[rep]"   #'+vertico:project-search)
  (evil-ex-define-cmd "pg[grep]d" #'+vertico:project-search-from-cwd))


;;
;;; Project tools

(evil-ex-define-cmd "com[pile]"   #'+evil:compile)
(evil-ex-define-cmd "make"        #'+evil:make)
(evil-ex-define-cmd "mk"          #'+evil:make) ; convenience alias
(evil-ex-define-cmd "debug"       #'+debugger/start)
(evil-ex-define-cmd "er[rors]"    #'+default/diagnostics)


;;
;;; File operations

(evil-ex-define-cmd "cp"        #'+evil:copy-this-file)
(evil-ex-define-cmd "mv"        #'+evil:move-this-file)
(evil-ex-define-cmd "rm"        #'+evil:delete-this-file)


;;
;;; :ui workspaces

(static-when (modulep! :ui workspaces)
  (evil-ex-define-cmd "sclear"      #'+workspace/kill-session)
  (evil-ex-define-cmd "sl[oad]"     #'zenit/quickload-session)
  (evil-ex-define-cmd "ss[ave]"     #'zenit/quicksave-session)
  (evil-ex-define-cmd "tabc[lose]"  #'+workspace:delete)
  (evil-ex-define-cmd "tabclear"    #'zenit/kill-all-buffers)
  (evil-ex-define-cmd "tabl[ast]"   #'+workspace/switch-to-last)
  (evil-ex-define-cmd "tabload"     #'+workspace:load)
  (evil-ex-define-cmd "tabn[ew]"    #'+workspace:new)
  (evil-ex-define-cmd "tabnext"     #'+workspace:switch-next)
  (evil-ex-define-cmd "tabprev"     #'+workspace:switch-previous)
  (evil-ex-define-cmd "tabr[ename]" #'+workspace:rename)
  (evil-ex-define-cmd "tabs"        #'+workspace/display)
  (evil-ex-define-cmd "tabsave"     #'+workspace:save))


;;
;;; `org-mode'

(static-when (modulep! :emacs org)
  (evil-ex-define-cmd "cap[ture]"   #'org-capture))


;;
;;; `ibuffer'

(static-when (modulep! :emacs ibuffer)
  (evil-ex-define-cmd "buffers" #'ibuffer))
