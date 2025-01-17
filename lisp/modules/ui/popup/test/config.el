;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; ui/popup/test/config.el

(require 'zenit-test)
(require 'zenit-modules)
(require 'el-patch)
(zenit-load (zenit-module-locate-path :ui 'popup "autoload/popup.el"))
(zenit-load (zenit-module-locate-path :ui 'popup "autoload/settings.el"))
(zenit-load (zenit-module-locate-path :ui 'popup "config.el"))

(zenit-deftest +popup--internal
  (:doc "`+popup--internal' is defined")
  (should (boundp '+popup--internal)))

(zenit-deftest +popup-window-parameters
  (:doc "`+popup-window-parameters' is defined")
  (should (boundp '+popup-window-parameters)))

(zenit-deftest +popup-default-display-buffer-actions
  (:doc "`+popup-default-display-buffer-actions' is defined")
  (should (boundp '+popup-default-display-buffer-actions)))

(zenit-deftest +popup-default-alist
  (:doc "`+popup-default-alist' is defined")
  (should (boundp '+popup-default-alist)))

(zenit-deftest +popup-default-parameters
  (:doc "`+popup-default-parameters' is defined")
  (should (boundp '+popup-default-parameters)))

(zenit-deftest +popup-margin-width
  (:doc "`+popup-margin-width' is defined")
  (should (boundp '+popup-margin-width)))

(zenit-deftest +popup-reference-buffers
  (:doc "`+popup-reference-buffers' is defined")
  (should (boundp '+popup-reference-buffers)))

(zenit-deftest +popup-buffer-status
  (:doc "`+popup-buffer-status' is defined")
  (should (boundp '+popup-buffer-status)))

(zenit-deftest +popup-group-function
  (:doc "`+popup-group-function' is defined")
  (should (boundp '+popup-group-function)))

(zenit-deftest +popup-open-buffers-alist
  (:doc "`+popup-open-buffers-alist' is defined")
  (should (boundp '+popup-open-buffers-alist)))

(zenit-deftest +popup-buried-buffers-alist
  (:doc "`+popup-buried-buffers-alist' is defined")
  (should (boundp '+popup-buried-buffers-alist)))

(zenit-deftest +popup--inhibit-transient
  (:doc "`+popup--inhibit-transient' is defined")
  (should (boundp '+popup--inhibit-transient)))

(zenit-deftest +popup--inhibit-select
  (:doc "`+popup--inhibit-select' is defined")
  (should (boundp '+popup--inhibit-select)))

(zenit-deftest +popup--old-display-buffer-alist
  (:doc "`+popup--old-display-buffer-alist' is defined")
  (should (boundp '+popup--old-display-buffer-alist)))

(zenit-deftest +popup--old-reference-buffers
  (:doc "`+popup--old-reference-buffers' is defined")
  (should (boundp '+popup--old-reference-buffers)))

(zenit-deftest +popup--remember-last
  (:doc "`+popup--remember-last' is defined")
  (should (boundp '+popup--remember-last)))

(zenit-deftest +popup--parents
  (:doc "`+popup--parents' is defined")
  (should (boundp '+popup--parents)))

(zenit-deftest +popup--ignore-parent
  (:doc "`+popup--ignore-parent' is defined")
  (should (boundp '+popup--ignore-parent)))

(zenit-deftest +popup--timer
  (:doc "`+popup--timer' is defined")
  (should (boundp '+popup--timer)))

(zenit-deftest +popup-mode-map
  (:doc "`+popup-mode-map' is defined")
  (should (boundp '+popup-mode-map)))

(zenit-deftest +popup-buffer-mode-map
  (:doc "`+popup-buffer-mode-map' is defined")
  (should (boundp '+popup-buffer-mode-map)))

(zenit-deftest +popup-mode
  (:doc "`+popup-mode' is defined")
  (progn
    (should (fboundp '+popup-mode))
    (should (boundp '+popup-mode))))

(zenit-deftest +popup-buffer--kill-last-tab-h
  (:doc "`+popup-buffer--kill-last-tab-h' is defined")
  (should (fboundp '+popup-buffer--kill-last-tab-h)))

(zenit-deftest +tab-line-temp-undedicate-win-a
  (:doc "`+tab-line-temp-undedicate-win-a' advises `tab-line-select-tab-buffer'")
  (progn
    (should (fboundp '+tab-line-temp-undedicate-win-a))
    (should (advice-member-p '+tab-line-temp-undedicate-win-a #'tab-line-select-tab-buffer))))

(zenit-deftest +popup-buffer-mode
  (:doc "`+popup-buffer-mode' is defined")
  (progn
    (should (fboundp '+popup-buffer-mode))
    (should (boundp '+popup-buffer-mode))))

(zenit-deftest zenit-init-ui-hook
  (:doc "`zenit-init-ui-hook' contains specified functions")
  (should (member '+popup-mode zenit-init-ui-hook)))

(zenit-deftest +popup-buffer-mode-hook
  (:doc "`+popup-buffer-mode-hook' contains specified functions")
  (dolist (hook-fn '(+popup-adjust-fringes-h +popup-adjust-margins-h
                     +popup-set-modeline-on-enable-h +popup-unset-modeline-on-disable-h))
    (should (member hook-fn +popup-buffer-mode-hook))))
