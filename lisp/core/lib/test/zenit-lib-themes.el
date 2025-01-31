;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/lib/test/zenit-lib-themes.el

(require 'zenit-test)
(zenit-require 'zenit-lib 'themes)

(zenit-deftest zenit-customize-theme-hook
  (:doc "`zenit-customize-theme-hook' is defined")
  (should (boundp 'zenit-customize-theme-hook)))

(zenit-deftest zenit--run-customize-theme-hook
  (:doc "`zenit--run-customize-theme-hook' is defined")
  (should (fboundp 'zenit--run-customize-theme-hook)))

(zenit-deftest zenit-apply-customized-faces-h
  (:doc "`zenit-apply-customized-faces-h' is a member of `zenit-load-theme-hook'")
  (progn
    (should (fboundp 'zenit-apply-customized-faces-h))
    (should (member #'zenit-apply-customized-faces-h zenit-load-theme-hook))))

(zenit-deftest zenit--normalize-face-spec ()
  (should (equal ',out (zenit--normalize-face-spec ',in)))
  (in out)
  :doc "`zenit--normalize-face-spec' handles a single face with keyword properties"
  (example-face :foreground "white" :background "black")
  ((example-face ((t (:foreground "white" :background "black")))))
  :doc "`zenit--normalize-face-spec' handles multiple faces with keyword properties"
  ((example-face1 example-face2) :foreground "white" :background "black")
  ((example-face1 ((t (:foreground "white" :background "black"))))
   (example-face2 ((t (:foreground "white" :background "black")))))
  :doc "`zenit--normalize-face-spec' handles a single face with a property list"
  (example-face (:foreground "white" :background "black"))
  ((example-face ((:foreground "white" :background "black")))))

(zenit-deftest custom-theme-set-faces!
  (:doc "`custom-theme-set-faces!' is defined")
  (should (fboundp 'custom-theme-set-faces!)))

(zenit-deftest custom-set-faces!
  (:doc "`custom-set-faces!' is defined")
  (should (fboundp 'custom-set-faces!)))

(zenit-deftest zenit/reload-theme
  (:doc "`zenit/reload-theme' is defined")
  (should (fboundp 'zenit/reload-theme)))
