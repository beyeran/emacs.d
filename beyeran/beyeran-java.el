
(require 'javarun)
(add-hook 'java-mode-hook (lambda () (javarun-mode 1)))
(setq javarun-java-path "/usr/bin")

(provide 'beyeran-java)
