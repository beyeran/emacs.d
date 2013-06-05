
(autoload 'octave-mode "octave-mod" nil t)

(sys-diversification ()
  ()
  (setq inferior-octave-program "/Applications/Octave.app/Contents/Resources/bin/octave"))

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda () 
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(provide 'beyeran-octave)
