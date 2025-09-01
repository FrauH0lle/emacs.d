;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/llm/packages.el

(package! gptel :lockfile tools_llm)
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :lockfile tools_llm)
(package! mcp :lockfile tools_llm)

(package! macher
  :recipe (:host github :repo "kmontag/macher")
  :lockfile tools_llm)

(package! mevedel
  :recipe (:host github :repo "FrauH0lle/mevedel" :files ("*.el"))
  :lockfile tools_llm)
