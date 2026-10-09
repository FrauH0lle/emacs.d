;; tools/tree-sitter/config.el -*- lexical-binding: t; -*-

(defvar +tree-sitter--commit-field? nil)

;;
;;; Packages

(use-package! treesit
  :when (treesit-available-p)
  :defer t
  :preface
  (setq treesit-enabled-modes t)

  ;; Increase the highlighting/christmas tree
  (setq treesit-font-lock-level 4)

  ;; So we can prevent double-prompting later.
  (put 'treesit-auto-install-grammar 'permanent-local t)

  ;; HACK: Intercept all ts-mode major mode remappings so grammars can be
  ;;   dynamically checked and `treesit-auto-install-grammar' can be
  ;;   consistently respected (which isn't currently the case with the majority
  ;;   of ts-modes, even the built-in ones).
  (defadvice! +tree-sitter--maybe-remap-major-mode-a (fn mode)
    :around #'major-mode-remap
    (let ((mode (funcall fn mode)))
      (if-let* ((ts (get mode '+tree-sitter)) ; registered by `set-tree-sitter!'
                (fallback-mode (car ts)))
          (cond ((get mode '+tree-sitter-mode))
                ((not (eval-when-compile (treesit-available-p)))
                 (message "Treesit unavailable, falling back to `%S'" fallback-mode)
                 (put mode '+tree-sitter-mode fallback-mode))
                ((not (fboundp mode))
                 (message "Couldn't find `%S', falling back to `%S'" mode fallback-mode)
                 fallback-mode)
                ((and (or (eq treesit-enabled-modes t)
                          (memq fallback-mode treesit-enabled-modes))
                      ;; Lazily load autoloaded `treesit-language-source-alist'
                      ;; entries.
                      (let ((fn (symbol-function mode))
                            ;; Silence "can't find grammar" warning popups from
                            ;; `treesit-ready-p' calls in Emacs <=30.1. We'll
                            ;; log it to *Messages* instead.
                            (warning-suppress-types
                             (if zenit-debug-mode
                                 warning-suppress-types
                               (cons '(treesit) warning-suppress-types)))
                            ;; For ts-modes that aren't registered with
                            ;; `set-tree-sitter!' and try to clobber these
                            ;; alists at load time.
                            auto-mode-alist
                            interpreter-mode-alist)
                        (or (not (autoloadp fn))
                            (autoload-do-load fn mode)))
                      ;; Only prompt once, and log other times.
                      (or (null (cdr ts))  ; no grammars, no problem!
                          ;; If the base/fallback mode doesn't exist, let's
                          ;; assume we want no fallthrough for this major mode
                          ;; and push forward anyway, even if a missing grammar
                          ;; results in a broken state.
                          (not (fboundp fallback-mode))
                          ;; Ensure grammars are present (and prompt to install
                          ;; them if needed).
                          (if-let* ((grammars
                                     (cl-loop for g in (cdr ts)
                                              unless (treesit-ready-p g 'message)
                                              collect g)))
                              (if (or (eq treesit-auto-install-grammar 'always)
                                      (if (eq treesit-auto-install-grammar 'ask)
                                          (and (not non-essential)
                                               (y-or-n-p
                                                (format "Missing tree-sitter grammars: %s\nInstall now?"
                                                        (mapconcat #'symbol-name grammars ", "))))))
                                  (always
                                   (cl-loop for grammar in grammars
                                            do (treesit-install-language-grammar grammar)
                                            finally do (setq-local treesit-auto-install-grammar t)))
                                (message "Treesit grammars missing (%s), falling back to `%s'..."
                                         (mapconcat #'symbol-name grammars ", ")
                                         fallback-mode)
                                nil)
                            t)))
                 (put mode '+tree-sitter-mode mode))
                (fallback-mode))
        mode)))
  :config
  (let ((data-dir (file-name-concat zenit-data-dir "tree-sitter")))
    (cl-pushnew data-dir treesit-extra-load-path :test #'equal)
    (make-directory data-dir t)
    ;; HACK: treesit lacks any way to dictate where to install grammars.
    (defadvice! +tree-sitter--install-grammar-to-local-dir-a (fn lang &optional out-dir &rest args)
      :around #'treesit-install-language-grammar
      :around #'treesit--build-grammar
      (apply fn lang (or out-dir data-dir) args)))

  (setq +tree-sitter--commit-field?
        (eq (cdr (func-arity
                  (advice--cd*r
                   (advice--symbol-function 'treesit--install-language-grammar-1))))
            'many))

  (cl-defun +tree-sitter-source (name &key url rev source-dir cc cpp commit)
    (cons name
          (append (list url rev source-dir cc cpp)
                  ;; COMPAT: 31.1 introduced a COMMIT recipe argument.  On
                  ;;   <=30.x, extra arguments will trigger an arity error when
                  ;;   installing grammars.
                  (if +tree-sitter--commit-field?
                      (list commit)))))

  (dolist (map `(;; Module-less (or major-mode-less) grammars
                 (awk :url "https://github.com/Beaglefoot/tree-sitter-awk")
                 (bibtex :url "https://github.com/latex-lsp/tree-sitter-bibtex")
                 (blueprint :url "https://github.com/huanie/tree-sitter-blueprint")
                 (cl-format :url "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
                            :source-dir "grammars/format/src")
                 (common-lisp :url "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
                              :source-dir "grammars/cl/src")
                 (latex :url "https://github.com/latex-lsp/tree-sitter-latex") ; see latex-lsp/tree-sitter-latex#172
                 (make :url "https://github.com/tree-sitter-grammars/tree-sitter-make")
                 (nu :url "https://github.com/nushell/tree-sitter-nu")
                 (org :url "https://github.com/milisims/tree-sitter-org")
                 (perl :url "https://github.com/ganezdragon/tree-sitter-perl")
                 (proto :url "https://github.com/mitchellh/tree-sitter-proto")
                 (r :url "https://github.com/r-lib/tree-sitter-r")
                 (sql :url "https://github.com/DerekStride/tree-sitter-sql" :rev "gh-pages")
                 (surface :url "https://github.com/connorlay/tree-sitter-surface")
                 (toml :url "https://github.com/tree-sitter-grammars/tree-sitter-toml"
                       :rev "v0.7.0")
                 (typst :url "https://github.com/uben0/tree-sitter-typst"
                        :rev "master"
                        :source-dir "src")
                 (systemverilog :url "https://github.com/gmlarumbe/tree-sitter-systemverilog")
                 (vhdl :url "https://github.com/alemuller/tree-sitter-vhdl")
                 (vue :url "https://github.com/tree-sitter-grammars/tree-sitter-vue")
                 (wast :url "https://github.com/wasm-lsp/tree-sitter-wasm"
                       :source-dir "wast/src")
                 (wat :url "https://github.com/wasm-lsp/tree-sitter-wasm"
                      :source-dir "wat/src")
                 (wgsl :url "https://github.com/mehmetoguzderin/tree-sitter-wgsl")

                 ;; Grammars with modules
                 (ada :url "https://github.com/briot/tree-sitter-ada")
                 (c :url "https://github.com/tree-sitter/tree-sitter-c"
                    :rev "v0.24.1")
                 (cpp :url "https://github.com/tree-sitter/tree-sitter-cpp")
                 (cmake :url "https://github.com/uyha/tree-sitter-cmake")
                 (c-sharp :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
                          :rev "v0.23.1")
                 (clojure :url "https://github.com/sogaiu/tree-sitter-clojure.git"
                          :rev "unstable-20250526")
                 (cuda :url "https://github.com/tree-sitter-grammars/tree-sitter-cuda")
                 (css :url "https://github.com/tree-sitter/tree-sitter-css"
                      :rev "v0.23.2")
                 (dart :url "https://github.com/UserNobody14/tree-sitter-dart")
                 (dockerfile :url "https://github.com/camdencheek/tree-sitter-dockerfile")
                 (doxygen :url "https://github.com/tree-sitter-grammars/tree-sitter-doxygen")
                 (elixir :url "https://github.com/elixir-lang/tree-sitter-elixir")
                 (erlang :url "https://github.com/WhatsApp/tree-sitter-erlang")
                 (fsharp :url "https://github.com/ionide/tree-sitter-fsharp"
                         :rev "v0.2.0")
                 (gdscript :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                           :rev,"v6.1.0")
                 (glsl :url "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
                 (graphql :url "https://github.com/bkegley/tree-sitter-graphql")
                 (go :url "https://github.com/tree-sitter/tree-sitter-go"
                     :rev "v0.25.0")
                 (gomod :url "https://github.com/camdencheek/tree-sitter-go-mod")
                 (gowork :url "https://github.com/omertuc/tree-sitter-go-work")
                 (gpr :url "https://github.com/brownts/tree-sitter-gpr")
                 (haskell :url "https://github.com/tree-sitter/tree-sitter-haskell")
                 (heex :url "https://github.com/phoenixframework/tree-sitter-heex")
                 (html :url "https://github.com/tree-sitter/tree-sitter-html"
                       :rev "v0.23.2")
                 (janet-simple :url "https://github.com/sogaiu/tree-sitter-janet-simple"
                               :cc ,(if (featurep :system 'windows) "gcc.exe"))
                 (java :url "https://github.com/tree-sitter/tree-sitter-java")
                 (javascript :url "https://github.com/tree-sitter/tree-sitter-javascript"
                             :rev "v0.25.0")
                 (jsdoc :url "https://github.com/tree-sitter/tree-sitter-jsdoc"
                        :rev "v0.23.2")
                 (json :url "https://github.com/tree-sitter/tree-sitter-json")
                 (julia :url "https://github.com/tree-sitter/tree-sitter-julia")
                 (kotlin :url "https://github.com/fwcd/tree-sitter-kotlin")
                 (lua :url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
                      :rev "v0.3.0")
                 (markdown :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                           :rev "v0.5.3"
                           :source-dir "tree-sitter-markdown/src")
                 (markdown-inline :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                                  :rev "v0.5.3"
                                  :source-dir "tree-sitter-markdown-inline/src")
                 (nix :url "https://github.com/nix-community/tree-sitter-nix")
                 (odin :url "https://github.com/tree-sitter-grammars/tree-sitter-odin"
                       :rev "v1.3.0")
                 (openscad :url "https://github.com/openscad/tree-sitter-openscad"
                           :rev "v0.7.1")
                 (php :url "https://github.com/tree-sitter/tree-sitter-php"
                      :rev "v0.23.11"
                      :source-dir "php/src")
                 (phpdoc :url "https://github.com/claytonrcarter/tree-sitter-phpdoc")
                 (python :url "https://github.com/tree-sitter/tree-sitter-python"
                         :rev "v0.25.0")
                 (regex :url "https://github.com/tree-sitter/tree-sitter-regex"
                        :rev "v0.24.3")
                 (ruby :url "https://github.com/tree-sitter/tree-sitter-ruby")
                 (rust :url "https://github.com/tree-sitter/tree-sitter-rust"
                       :rev "v0.24.2")
                 (scala :url "https://github.com/tree-sitter/tree-sitter-scala")
                 (sml :url "https://github.com/MatthewFluet/tree-sitter-sml")
                 (swift :url "https://github.com/alex-pinkus/tree-sitter-swift"
                        :rev "0.7.1-with-generated-files")
                 (typescript :url "https://github.com/tree-sitter/tree-sitter-typescript"
                             :source-dir "typescript/src")
                 (tsx :url "https://github.com/tree-sitter/tree-sitter-typescript"
                      :source-dir "tsx/src")
                 (qmljs :url "https://github.com/yuja/tree-sitter-qmljs")
                 (yaml :url "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
                       :rev "v0.7.2")
                 (zig :url "https://github.com/tree-sitter-grammars/tree-sitter-zig")))
    (cl-pushnew (apply #'+tree-sitter-source map)
                treesit-language-source-alist
                :key #'car
                :test #'eq)))


(use-package! evil-textobj-tree-sitter
  :when (modulep! :editor evil)
  :defer t
  :init (after! (evil treesit) (require 'evil-textobj-tree-sitter))
  :config
  (after! evil
    (defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))
    (defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))
    (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
    (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

    (evil-define-key '(visual operator) 'tree-sitter-mode
      "i" +tree-sitter-inner-text-objects-map
      "a" +tree-sitter-outer-text-objects-map)
    (evil-define-key 'normal 'tree-sitter-mode
      "[g" +tree-sitter-goto-previous-map
      "]g" +tree-sitter-goto-next-map)

    (map! (:map +tree-sitter-inner-text-objects-map
                "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
                "f" (+tree-sitter-get-textobj "function.inner")
                "F" (+tree-sitter-get-textobj "call.inner")
                "C" (+tree-sitter-get-textobj "class.inner")
                "v" (+tree-sitter-get-textobj "conditional.inner")
                "l" (+tree-sitter-get-textobj "loop.inner"))
          (:map +tree-sitter-outer-text-objects-map
                "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
                "f" (+tree-sitter-get-textobj "function.outer")
                "F" (+tree-sitter-get-textobj "call.outer")
                "C" (+tree-sitter-get-textobj "class.outer")
                "c" (+tree-sitter-get-textobj "comment.outer")
                "v" (+tree-sitter-get-textobj "conditional.outer")
                "l" (+tree-sitter-get-textobj "loop.outer"))

          (:map +tree-sitter-goto-previous-map
                "a" (+tree-sitter-goto-textobj "parameter.outer" t)
                "f" (+tree-sitter-goto-textobj "function.outer" t)
                "F" (+tree-sitter-goto-textobj "call.outer" t)
                "C" (+tree-sitter-goto-textobj "class.outer" t)
                "c" (+tree-sitter-goto-textobj "comment.outer" t)
                "v" (+tree-sitter-goto-textobj "conditional.outer" t)
                "l" (+tree-sitter-goto-textobj "loop.outer" t))
          (:map +tree-sitter-goto-next-map
                "a" (+tree-sitter-goto-textobj "parameter.outer")
                "f" (+tree-sitter-goto-textobj "function.outer")
                "F" (+tree-sitter-goto-textobj "call.outer")
                "C" (+tree-sitter-goto-textobj "class.outer")
                "c" (+tree-sitter-goto-textobj "comment.outer")
                "v" (+tree-sitter-goto-textobj "conditional.outer")
                "l" (+tree-sitter-goto-textobj "loop.outer")))

    (after! which-key
      (setq which-key-allow-multiple-replacements t)
      (pushnew!
       which-key-replacement-alist
       '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))))))
