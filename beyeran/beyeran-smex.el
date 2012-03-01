
;;;;;;; smex ;;;;;;;
(and (require 'ido "ido" t)
     (ido-mode t)
     (require 'smex "smex" t)
     (smex-initialize)
     (setq smex-save-file "~/.smex")
     (smex-auto-update))

(provide 'beyeran-smex)
