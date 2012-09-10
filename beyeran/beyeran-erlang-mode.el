
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.7/emacs/" load-path)
      erlang-root-dir "/usr/lib/erlang/"
          exec-path (cons "/usr/bin/" exec-path))

(require 'erlang-start)

(provide 'beyeran-erlang-mode)
