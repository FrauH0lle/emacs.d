;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-fonts.el

(describe "core/lib/fonts"

  (load! "lib/fonts" zenit-core-dir)

  (defvar zenit-font (font-spec :family "my font" :size 12))
  (defvar zenit-serif-font (font-spec :family "my font" :size 12))
  (defvar zenit-variable-pitch-font (font-spec :family "my font" :size 12))

  (autoload 'x-decompose-font-name "fontset")

  (describe "zenit-normalize-font"
    :var (font normalized-font)

    (before-each
      (spy-on 'x-resolve-font-name :and-return-value "-FSD -my font-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
      (spy-on 'font-info :and-return-value ["-FSD -my font-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1"]))

    (it "normalizes a font-spec"
      (setq font (font-spec :family "my font" :size 12))
      (setq normalized-font (zenit-normalize-font font))

      (expect (symbol-name (font-get normalized-font :family)) :to-equal "my font")
      (expect (font-get normalized-font :size) :to-equal 12)
      (expect (font-get normalized-font :weight) :to-equal 'normal)
      (expect (font-get normalized-font :slant) :to-equal 'normal)
      (expect (font-get normalized-font :width) :to-equal 'normal))

    (it "normalizes an XFT font string"
      (setq font "my font-12")
      (setq normalized-font (zenit-normalize-font font))

      (expect (symbol-name (font-get normalized-font :family)) :to-equal "my font")
      (expect (font-get normalized-font :size) :to-equal 12)
      (expect (font-get normalized-font :weight) :to-equal 'normal)
      (expect (font-get normalized-font :slant) :to-equal 'normal)
      (expect (font-get normalized-font :width) :to-equal 'normal))

    (it "normalizes an XLFD font string"
      (setq font "-*-my font-medium-r-normal--12-*-*-*-*-*-iso10646-1")
      (setq normalized-font (zenit-normalize-font font))

      (expect (symbol-name (font-get normalized-font :family)) :to-equal "my font")
      (expect (font-get normalized-font :size) :to-equal 12)
      (expect (font-get normalized-font :weight) :to-equal 'normal)
      (expect (font-get normalized-font :slant) :to-equal 'normal)
      (expect (font-get normalized-font :width) :to-equal 'normal)))


  (describe "zenit-adjust-font-size"

    (before-each
      (spy-on 'display-multi-font-p :and-return-value t)
      (spy-on 'x-resolve-font-name :and-return-value "-FSD -my font-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1")
      (spy-on 'font-info :and-return-value ["-FSD -my font-normal-normal-normal-*-12-*-*-*-*-0-iso10646-1"])
      (spy-on 'zenit-init-fonts-h)
      (set-face-attribute 'default nil :font zenit-font))

    (it "increases the font size"
      (zenit-adjust-font-size 2)
      (expect (font-get zenit-font :size) :to-equal 16))

    (it "decreases the font size"
      (zenit-adjust-font-size -2)
      (expect (font-get zenit-font :size) :to-equal 8)))


  (describe "zenit/reload-font"
    (before-each
      (spy-on 'zenit-init-fonts-h))
    (it "calls zenit-init-fonts-h"
      (zenit/reload-font)
      (expect 'zenit-init-fonts-h :to-have-been-called-with 'reload)))


  (describe "zenit/increase-font-size"
    :var (zenit-font-increment)
    (before-each
      (spy-on 'zenit-adjust-font-size)
      (setq zenit-font-increment 2))
    (it "increases the font size via zenit-adjust-font-size"
      (zenit/increase-font-size 1)
      (expect 'zenit-adjust-font-size :to-have-been-called-with 2)))


  (describe "zenit/decrease-font-size"
    :var (zenit-font-increment)

    (before-each
      (spy-on 'zenit-adjust-font-size)
      (setq zenit-font-increment 2))

    (it "decreases the font size via zenit-adjust-font-size"
      (zenit/decrease-font-size 1)
      (expect 'zenit-adjust-font-size :to-have-been-called-with -2)))


  (describe "zenit/reset-font-size"
    :var (zenit-big-font-mode)

    (before-each
      (spy-on 'zenit-adjust-font-size :and-return-value t)
      (spy-on 'zenit-big-font-mode)
      (setq zenit-big-font-mode nil))

    (it "resets the font size via zenit-adjust-font-size"
      (zenit/reset-font-size)
      (expect 'zenit-adjust-font-size :to-have-been-called-with nil))

    (it "switches zenit-big-font-mode off"
      (setq zenit-big-font-mode t)
      (quiet! (zenit/reset-font-size))
      (expect 'zenit-big-font-mode :to-have-been-called-with -1)))



  (describe "zenit-big-font-mode"
    :var (zenit-big-font zenit-big-font-increment)

    (before-each
      (setq zenit-big-font-increment 4)
      (spy-on 'zenit-adjust-font-size))

    (it "resizes the font when activated"
      (zenit-big-font-mode +1)
      (expect 'zenit-adjust-font-size :to-have-been-called-with (if zenit-big-font-mode zenit-big-font-increment)))

    (it "resizes the font when deactivated"
      (setq zenit-big-font-mode t)
      (zenit-big-font-mode -1)
      (expect 'zenit-adjust-font-size :to-have-been-called-with (if zenit-big-font-mode zenit-big-font-increment)))))
