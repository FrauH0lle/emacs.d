;; checkers/grammar/config.el -*- lexical-binding: t; -*-

(use-package! langtool
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (unless (or langtool-bin
              langtool-language-tool-jar
              langtool-java-classpath)
    (cond ((setq langtool-bin (executable-find "languagetool")))
          (zenit--system-macos-p
           (cond
            ;; is user using home brew?
            ((file-directory-p "/usr/local/Cellar/languagetool")
             (setq langtool-language-tool-jar
                   (locate-file "libexec/languagetool-commandline.jar"
                                (zenit-files-in "/usr/local/Cellar/languagetool"
                                                :type 'dirs
                                                :depth 2))))
            ;; macports compatibility
            ((file-directory-p "/opt/local/share/java/LanguageTool")
             (setq langtool-java-classpath "/opt/local/share/java/LanguageTool/*"))))
          (zenit--system-linux-p
           (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")))))
