;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/llm/packages.el

(package! gptel :lockfile tools_llm)
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :lockfile tools_llm)
(when (modulep! :tools magit)
  (package! gptel-magit :lockfile tools_llm_magit))

(package! aidermacs :lockfile tools_llm)
