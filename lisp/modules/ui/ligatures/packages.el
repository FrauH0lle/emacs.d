;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/ligatures/packages.el

(when (and (or (featurep 'ns)
               (string-match-p "HARFBUZZ" system-configuration-features))
           (featurep 'composite))
  (package! ligature :lockfile ui-ligatures))
