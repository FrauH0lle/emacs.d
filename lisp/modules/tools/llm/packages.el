;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; tools/llm/packages.el

(package! gptel :recipe (:nonrecursive t) :lockfile tools_llm)
(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :lockfile tools_llm)
(package! mcp :lockfile tools_llm)
(package! mevedel
  :recipe (:host github
           :repo "FrauH0lle/mevedel"
           :files ("*.el"
                   ("native" "native/*.c" "native/*.eld")
                   ("scripts" "scripts/mevedel-mcp-stdio.py")
                   "agents"
                   "prompts"
                   "skills"
                   ("shared-editing"
                    "shared-editing/host.bundle.mjs"
                    "shared-editing/resvg.wasm"
                    "shared-editing/font.ttf"
                    "shared-editing/Excalifont.ttf"
                    "shared-editing/Nunito.ttf"
                    "shared-editing/ComicShanns.ttf"
                    "shared-editing/builtin.excalidrawlib"
                    "shared-editing/FONT-LICENSE"
                    "shared-editing/RESVG-LICENSE"
                    "shared-editing/THIRD-PARTY-NOTICES.txt")
                   "docs/*.md"
                   "docs/adr/*.md"
                   "docs/agents/*.md"
                   "tools"))
  :lockfile tools_llm)
