;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lang/python/packages.el

;; Major modes
(package! pip-requirements :lockfile lang-python)
(when (modulep! +cython)
  (package! cython-mode :lockfile lang-python)
  (when (modulep! :checkers syntax)
    (package! flycheck-cython :lockfile lang-python)))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (when (modulep! +pyright)
      (package! lsp-pyright :lockfile lang-python))))

;; Programming environment
(package! anaconda-mode)

;; Environment management
(package! pipenv :lockfile lang-python)
(package! pyvenv :lockfile lang-python)
(when (modulep! +pyenv)
  (package! pyenv-mode :lockfile lang-python))
(when (modulep! +conda)
  (package! conda :lockfile lang-python))
(when (modulep! +poetry)
  (package! poetry :lockfile lang-python))

;; Testing frameworks
(package! nose :lockfile lang-python)
(package! python-pytest :lockfile lang-python)

;; Import managements
(package! pyimport :lockfile lang-python)
(package! py-isort :lockfile lang-python)
