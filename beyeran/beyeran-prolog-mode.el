
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setq prolog-system 'swi
      auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist)
          prolog-program-name "/usr/bin/gprolog")

(provide 'beyeran-prolog-mode)
