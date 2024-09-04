;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/ligatures/packages.el

(when (and (or (featurep 'ns)
               (featurep 'harfbuzz))
           (featurep 'composite))
  (package! ligature :lockfile ui_ligatures))
