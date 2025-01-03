;; ui/transient-state/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+symbol-overlay-transient-state "ui/transient-state/autoload" nil t)
(transient-define-prefix +symbol-overlay-transient-state ()
  [:description
   "Symbol overlay\n"
   ["Navigation"
    ("p" "Previous match" symbol-overlay-jump-prev
     :transient t)
    ("n" "Next match" symbol-overlay-jump-next
     :transient t)
    ("d" "Jump to definition" symbol-overlay-jump-to-definition)]

   ["Edit"
    ("r" "Rename symbol" symbol-overlay-rename)
    ("t" "Toggle in scope" symbol-overlay-toggle-in-scope
     :description
     (lambda ()
       (format "%s %s" (if symbol-overlay-scope "[x]" "[ ]") "In Scope"))
     :transient t)]

   ["Search"
    (+so-ts--search-buffer)
    (+so-ts--search-all-buffers)
    (+so-ts--search-files)
    ("/" "Project" +default/search-project-for-symbol-at-point)]]

  [:class transient-row
          ("z" "Recenter" recenter-top-bottom :transient t)
          (+symbol-overlay-transient-state-quit)]
  (interactive)
  (symbol-overlay-put)
  (add-hook! 'transient-exit-hook :local t (symbol-overlay-put))
  (transient-setup '+symbol-overlay-transient-state))

(transient-define-suffix +so-ts--search-buffer ()
  "Suffix to call `+vertico/search-symbol-at-point'."
  :transient nil
  :key "s"
  :description "Buffer"
  (interactive)
  (+vertico/search-symbol-at-point))

(transient-define-suffix +so-ts--search-all-buffers ()
  "Suffix to call `+vertico/search-symbol-at-point' with arg."
  :transient nil
  :key "b"
  :description "All buffers"
  (interactive)
  (+vertico/search-symbol-at-point t))

(transient-define-suffix +so-ts--search-files ()
  "Suffix to call `+vertico-file-search'."
  :transient nil
  :key "f"
  :description "Files"
  (interactive)
  (+vertico-file-search :query (thing-at-point 'symbol)))

(transient-define-suffix +symbol-overlay-transient-state-quit ()
  "Suffix to call `transient-quit-all'."
  :transient nil
  :key "q"
  :description "Quit"
  (interactive)
  (transient-quit-one))
