;;
;; color theme
;;

(defvar color-themes-in-theme-directory (rest (rest (directory-files "~/.emacs.d/color-theme/"))))

(mapcar (lambda (n) (add-theme n))
        color-themes-in-theme-directory)

(load-theme 'assemblage t)

;;
;; hud
;;
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(toggle-frame-fullscreen)

;; stripping more
(setq initial-scratch-message "")
(setq visible-bell t)

;; hide modeline
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format mode-line-format nil)
    (setq mode-line-format hide-mode-line hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay th mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hide-mode-line-mode)
    (run-with-idle-timer 0 nil 'message
                         (concat "Hidden mode Line Mode enabled.   "
                                 "Use M-x hidden-mode-line-mode to make the mode line appear."))))

(hidden-mode-line-mode 1)
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)


;; big fringe mode
;; (defvar big-fringe-mode nil)
;; (define-minor-mode big-fringe-mode
;;   "Minor mode to use big fringe in the current buffer."
;;   :init-value nil
;;   :global t
;;   :variable big-fringe-mode
;;   :group 'editing-basics
;;   (if (not big-fringe-mode)
;;       (set-fringe-style nil)
;;     (set-fringe-mode
;;      (/ (- (frame-pixel-width)
;;            (* 100 (frame-char-width)))
;;         1))))


;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (if (delq nil
;;                       (let ((fw (frame-width)))
;;                         (mapcar (lambda (w) (< (window-width w) fw)) (window-list))))
;;                 (big-fringe-mode 0)
;;               (big-fringe-mode 1))))

;; (mapcar (lambda (fb) (set-fringe-bitmap-face fb 'org-hide))
;;         fringe-bitmaps)


(global-visual-line-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq inhibit-splash-screen t)
(setq visible-bell t)

;;
;; golden ratio
;;
(with-library golden-ratio)
(setq golden-ratio-auto-scale t)

(golden-ratio-mode 1)

;;
;; font
;;
(set-face-attribute 'default nil :font "Source Code Pro-10")
;; (set-default-font "Droid Sans Mono-9")
