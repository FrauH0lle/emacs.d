;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/llm/packages.el

(package! gptel :lockfile tools_llm)
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :lockfile tools_llm)
(package! mcp
  :recipe (:fork
           (:host github
            :repo "FrauH0lle/mcp.el"
            :branch "feat-synchronous-server-init"))
  :lockfile tools_llm)

(package! macher
  :recipe (:host github :repo "kmontag/macher")
  :lockfile tools_llm)

(package! macher-instruct
  :recipe (:host github :repo "FrauH0lle/macher-instruct" :protocol ssh :files ("*.el"))
  :lockfile tools_llm)
