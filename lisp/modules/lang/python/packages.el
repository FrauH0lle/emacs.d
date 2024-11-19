;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lang/python/packages.el

;; Major modes
(package! pip-requirements :lockfile lang_python)
(when (modulep! +cython)
  (package! cython-mode :lockfile lang_python)
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-cython :lockfile lang_python)))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (when (modulep! +pyright)
      (package! lsp-pyright :lockfile lang_python))))

;; Programming environment
(package! anaconda-mode)

;; Environment management
(package! pipenv :lockfile lang_python)
(package! pyvenv :lockfile lang_python)
(when (modulep! +pyenv)
  (package! pyenv-mode :lockfile lang_python))
(when (modulep! +conda)
  (package! conda :lockfile lang_python))
(when (modulep! +poetry)
  (package! poetry :lockfile lang_python))

;; Testing frameworks
(package! nose :lockfile lang_python)
(package! python-pytest :lockfile lang_python)

;; Import managements
(package! pyimport :lockfile lang_python)
(package! py-isort :lockfile lang_python)
