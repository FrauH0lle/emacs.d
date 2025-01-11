;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; lisp/core/test/test-lib-themes.el

(describe "core/lib/themes"

  (load! "lib/themes" zenit-core-dir)

  (before-each
    (setq zenit-theme nil))

  (describe "zenit--custom-theme-set-face"
    (it "handles a single face with keyword properties"
      (let ((spec '(example-face :foreground "white" :background "black")))
        (expect (zenit--custom-theme-set-face spec)
                :to-equal
                '((example-face ((t (:foreground "white" :background "black"))))))))

    (it "handles multiple faces with keyword properties"
      (let ((spec '((example-face1 example-face2) :foreground "white" :background "black")))
        (expect (zenit--custom-theme-set-face spec)
                :to-equal
                '((example-face1 ((t (:foreground "white" :background "black"))))
                  (example-face2 ((t (:foreground "white" :background "black"))))))))

    (it "handles a single face with a property list"
      (let ((spec '(example-face (:foreground "white" :background "black"))))
        (expect (zenit--custom-theme-set-face spec)
                :to-equal
                '((example-face ((:foreground "white" :background "black"))))))))


  (describe "zenit-apply-customized-faces-h"
    (it "is defined"
      (expect (fboundp 'zenit-apply-customized-faces-h) :to-be t)))


  (describe "custom-theme-set-faces!"
    (before-each
      (spy-on 'custom-theme-enabled-p :and-return-value t))

    (it "applies face specs to a single theme"
      (let ((theme 'test-theme)
            (face-specs '((example-face :foreground "white" :background "black"))))
        (spy-on 'zenit--custom-theme-set-face :and-return-value '((example-face ((t (:foreground "white" :background "black"))))))
        (spy-on 'custom-theme-set-faces)
        (custom-theme-set-faces! theme face-specs)
        (expect 'zenit--custom-theme-set-face :to-have-been-called-with face-specs)
        (expect 'custom-theme-set-faces :to-have-been-called-with theme '(example-face ((t (:foreground "white" :background "black")))))))

    (it "applies face specs to multiple themes"
      (let ((themes '(theme1 theme2))
            (face-specs '((example-face :foreground "white" :background "black"))))
        (spy-on 'zenit--custom-theme-set-face :and-return-value '((example-face ((t (:foreground "white" :background "black"))))))
        (spy-on 'custom-theme-set-faces)
        (custom-theme-set-faces! themes face-specs)
        (expect 'zenit--custom-theme-set-face :to-have-been-called-with face-specs)
        (expect 'custom-theme-set-faces :to-have-been-called-times 2)))

    (it "applies face specs to all themes when theme is nil"
      (let ((face-specs '((example-face :foreground "white" :background "black"))))
        (spy-on 'zenit--custom-theme-set-face :and-return-value '((example-face ((t (:foreground "white" :background "black"))))))
        (spy-on 'custom-theme-set-faces)
        (custom-theme-set-faces! nil face-specs)
        (expect 'zenit--custom-theme-set-face :to-have-been-called-with face-specs)
        (expect 'custom-theme-set-faces :to-have-been-called-with 'user '(example-face ((t (:foreground "white" :background "black"))))))))


  (describe "custom-set-faces!"
    (it "correctly expands into custom-theme-set-faces!"
      (expect '(custom-set-faces! ((example-face :foreground "white" :background "black")))
              :to-expand-into
              '(custom-theme-set-faces! 'user
                 ((example-face :foreground "white" :background "black"))))))


  (describe "zenit/reload-theme"
    (it "reloads the theme and font"
      (spy-on 'load-theme)
      (spy-on 'zenit/reload-font)
      (zenit/reload-theme)
      (expect 'load-theme :to-have-been-called-with zenit-theme 'noconfirm)
      (expect 'zenit/reload-font :to-have-been-called))))
