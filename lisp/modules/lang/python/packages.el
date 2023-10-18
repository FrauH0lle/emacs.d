;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lang/python/packages.el

;; Major modes
(package! pip-requirements)
(when (modulep! +cython)
  (package! cython-mode)
  (when (modulep! :checkers syntax)
    (package! flycheck-cython)))

;; LSP
(when (modulep! +lsp)
  (unless (modulep! :tools lsp +eglot)
    (if (modulep! +pyright)
        (package! lsp-pyright)
      (package! lsp-python-ms))))

;; Programming environment
(package! anaconda-mode)
(when (modulep! :completion company)
  (package! company-anaconda))

;; Environment management
(package! pipenv)
(package! pyvenv)
(when (modulep! +pyenv)
  (package! pyenv-mode))
(when (modulep! +conda)
  (package! conda))
(when (modulep! +poetry)
  (package! poetry))

;; Testing frameworks
(package! nose)
(package! python-pytest)

;; Import managements
(package! pyimport)
(package! py-isort)
