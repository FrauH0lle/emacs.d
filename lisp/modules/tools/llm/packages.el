;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/llm/packages.el

(package! gptel :lockfile tools_llm)
(package! aider
  :recipe (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :lockfile tools_llm)
