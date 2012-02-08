
(require 'ess-mode)
(require 'ess-site)
(require 'ess-eldoc)

(setq auto-mode-alist (append (list '("\\.S$" . S-mode)
                                    '("\\.s$" . S-mode)
                                    '("\\.R$" . R-mode)
                                    '("\\.r$" . R-mode))
                              auto-mode-alist))

(setq-default inferior-R-program-name "R")

(provide 'beyeran-ess)
