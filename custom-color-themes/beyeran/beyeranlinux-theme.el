
;;
;; file: beyeranlinux-theme.el
;;

;;;; color-theme ;;;;
(deftheme beyeranlinux
  "a custom theme for which the documentation is still up to be edited")

(let ((class '((class color) (min-colors 89)))
          ;; everything is completly taken from the zenburn theme
          ;; only the colors are changed
      (beyeranlinux-fg "#ffffff")
      (beyeranlinux-fg-1 "#c7f464")
      (beyeranlinux-bg-1 "#080808")
      (beyeranlinux-bg-05 "#556270")
      (beyeranlinux-bg "#383838")
      (beyeranlinux-bg+1 "#556270")
      (beyeranlinux-bg+2 "#556270")
      (beyeranlinux-bg+3 "#556270")
      (beyeranlinux-red+1 "#c44d58")
      (beyeranlinux-red   "#c44d58")
      (beyeranlinux-red-1 "#c44d58")
      (beyeranlinux-red-2 "#c44d58")
      (beyeranlinux-red-3 "#c44d58")
      (beyeranlinux-red-4 "#c44d58")
      (beyeranlinux-orange "#ff6b6b")
      (beyeranlinux-yellow "#c44d58")
      (beyeranlinux-yellow-1 "#c44d58")
      (beyeranlinux-yellow-2 "#c44d58")
      (beyeranlinux-green-1 "#5f7f5f")
      (beyeranlinux-green   "#7f9f7f")
      (beyeranlinux-green+1 "#8fb28f")
      (beyeranlinux-green+2 "#9fc59f")
      (beyeranlinux-green+3 "#afd8af")
      (beyeranlinux-green+4 "#bfebbf")
      (beyeranlinux-cyan "#93e0e3")
      (beyeranlinux-blue+1 "#4ecdc4")
      (beyeranlinux-blue "#4ecdc4")
      (beyeranlinux-blue-1 "#4ecdc4")
      (beyeranlinux-blue-2 "#4ecdc4")
      (beyeranlinux-blue-3 "#4ecdc4")
      (beyeranlinux-blue-4 "#4ecdc4")
      (beyeranlinux-blue-5 "#4ecdc4")
      (beyeranlinux-magenta "#dc8cc3"))
  (custom-theme-set-faces
   'beyeranlinux
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,beyeranlinux-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,beyeranlinux-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,beyeranlinux-fg :background ,beyeranlinux-bg))))
   `(cursor ((,class (:foreground ,beyeranlinux-fg))))
   `(escape-glyph-face ((,class (:foreground ,beyeranlinux-red))))
   `(fringe ((,class (:foreground ,beyeranlinux-fg :background ,beyeranlinux-bg+1))))
   `(header-line ((,class (:foreground ,beyeranlinux-yellow
                                       :background ,beyeranlinux-bg-1
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,beyeranlinux-bg-05))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,beyeranlinux-green))))
   `(compilation-error-face ((,class (:foreground ,beyeranlinux-red-1 :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,beyeranlinux-fg))))
   `(compilation-info-face ((,class (:foreground ,beyeranlinux-blue))))
   `(compilation-info ((,class (:foreground ,beyeranlinux-green+4 :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,beyeranlinux-green))))
   `(compilation-line-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(compilation-line-number ((,class (:foreground ,beyeranlinux-yellow))))
   `(compilation-message-face ((,class (:foreground ,beyeranlinux-blue))))
   `(compilation-warning-face ((,class (:foreground ,beyeranlinux-yellow-1 :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,beyeranlinux-fg))))
   `(grep-error-face ((,class (:foreground ,beyeranlinux-red-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,beyeranlinux-blue))))
   `(grep-match-face ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(match ((,class (:background ,beyeranlinux-bg-1 :foreground ,beyeranlinux-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,beyeranlinux-yellow :background ,beyeranlinux-bg-1))))
   `(isearch-fail ((,class (:foreground ,beyeranlinux-fg :background ,beyeranlinux-red-4))))
   `(lazy-highlight ((,class (:foreground ,beyeranlinux-yellow :background ,beyeranlinux-bg+2))))

   `(menu ((,class (:foreground ,beyeranlinux-fg :background ,beyeranlinux-bg))))
   `(minibuffer-prompt ((,class (:foreground ,beyeranlinux-yellow))))
   `(mode-line
     ((,class (:foreground ,beyeranlinux-green+1
                           :background ,beyeranlinux-bg-1
                           :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,beyeranlinux-green-1
                           :background ,beyeranlinux-bg-05
                           :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,beyeranlinux-bg-1))))
   `(secondary-selection ((,class (:background ,beyeranlinux-bg+2))))
   `(trailing-whitespace ((,class (:background ,beyeranlinux-red))))
   `(vertical-border ((,class (:foreground ,beyeranlinux-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,beyeranlinux-blue))))
   `(font-lock-comment-face ((,class (:foreground ,beyeranlinux-green))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,beyeranlinux-green))))
   `(font-lock-constant-face ((,class (:foreground ,beyeranlinux-green+4))))
   `(font-lock-doc-face ((,class (:foreground ,beyeranlinux-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,beyeranlinux-blue+1))))
   `(font-lock-function-name-face ((,class (:foreground ,beyeranlinux-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,beyeranlinux-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,beyeranlinux-blue))))
   `(font-lock-string-face ((,class (:foreground ,beyeranlinux-red))))
   `(font-lock-type-face ((,class (:foreground ,beyeranlinux-blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,beyeranlinux-orange))))
   `(font-lock-warning-face ((,class (:foreground ,beyeranlinux-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-default-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,beyeranlinux-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,beyeranlinux-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,beyeranlinux-green))))
   `(newsticker-new-item-face ((,class (:foreground ,beyeranlinux-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,beyeranlinux-red))))
   `(newsticker-old-item-face ((,class (:foreground ,beyeranlinux-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,beyeranlinux-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,beyeranlinux-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,beyeranlinux-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,beyeranlinux-red))))
   `(newsticker-treeview-old-face ((,class (:foreground ,beyeranlinux-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,beyeranlinux-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,beyeranlinux-fg))))
   `(ack-file ((,class (:foreground ,beyeranlinux-blue))))
   `(ack-line ((,class (:foreground ,beyeranlinux-yellow))))
   `(ack-match ((,class (:foreground ,beyeranlinux-orange :background ,beyeranlinux-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,beyeranlinux-yellow :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,beyeranlinux-bg+3 :foreground "black"))))
   `(ac-selection-face ((,class (:background ,beyeranlinux-blue-4 :foreground ,beyeranlinux-fg))))
   `(popup-tip-face ((,class (:background ,beyeranlinux-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,beyeranlinux-blue-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,beyeranlinux-bg-1))))
   `(popup-isearch-match ((,class (:background ,beyeranlinux-bg :foreground ,beyeranlinux-fg))))

   ;; diff
   `(diff-added ((,class (:foreground ,beyeranlinux-green+4))))
   `(diff-changed ((,class (:foreground ,beyeranlinux-yellow))))
   `(diff-removed ((,class (:foreground ,beyeranlinux-red))))
   `(diff-header ((,class (:background ,beyeranlinux-bg+2))))
   `(diff-file-header
     ((,class (:background ,beyeranlinux-bg+2 :foreground ,beyeranlinux-fg :bold t))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,beyeranlinux-green+4 :background ,beyeranlinux-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,beyeranlinux-red :background ,beyeranlinux-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,beyeranlinux-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,beyeranlinux-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,beyeranlinux-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,beyeranlinux-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,beyeranlinux-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,beyeranlinux-red-1 :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,beyeranlinux-yellow-1 :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,beyeranlinux-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,beyeranlinux-red-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,beyeranlinux-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,beyeranlinux-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(erc-keyword-face ((,class (:foreground ,beyeranlinux-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,beyeranlinux-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,beyeranlinux-green))))
   `(erc-pal-face ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,beyeranlinux-orange :background ,beyeranlinux-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,beyeranlinux-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,beyeranlinux-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,beyeranlinux-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,beyeranlinux-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,beyeranlinux-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,beyeranlinux-blue))))
   `(gnus-summary-low-read ((t (:foreground ,beyeranlinux-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,beyeranlinux-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,beyeranlinux-blue))))
   `(gnus-summary-normal-read ((,class (:foreground ,beyeranlinux-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,beyeranlinux-fg))))
   `(gnus-summary-selected ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,beyeranlinux-blue))))
   `(gnus-cite-10 ((,class (:foreground ,beyeranlinux-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,beyeranlinux-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,beyeranlinux-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,beyeranlinux-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,beyeranlinux-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,beyeranlinux-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,beyeranlinux-green))))
   `(gnus-cite-7 ((,class (:foreground ,beyeranlinux-red))))
   `(gnus-cite-8 ((,class (:foreground ,beyeranlinux-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,beyeranlinux-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,beyeranlinux-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,beyeranlinux-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,beyeranlinux-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,beyeranlinux-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,beyeranlinux-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,beyeranlinux-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,beyeranlinux-bg+2))))
   `(gnus-signature ((,class (:foreground ,beyeranlinux-yellow))))
   `(gnus-x ((,class (:background ,beyeranlinux-fg :foreground ,beyeranlinux-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,beyeranlinux-green
                           :background ,beyeranlinux-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,beyeranlinux-yellow
                           :background ,beyeranlinux-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,beyeranlinux-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,beyeranlinux-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,beyeranlinux-bg :background ,beyeranlinux-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,beyeranlinux-green+4 :background ,beyeranlinux-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,beyeranlinux-bg-1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(ido-only-match ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(ido-subdir ((,class (:foreground ,beyeranlinux-yellow))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,beyeranlinux-orange))))
   `(js2-error-face ((,class (:foreground ,beyeranlinux-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,beyeranlinux-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,beyeranlinux-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,beyeranlinux-green+3))))
   `(js2-function-param-face ((,class (:foreground, beyeranlinux-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,beyeranlinux-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,beyeranlinux-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,beyeranlinux-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,beyeranlinux-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,beyeranlinux-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,beyeranlinux-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,beyeranlinux-red+1))))
   `(jabber-activity-face((,class (:foreground ,beyeranlinux-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,beyeranlinux-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,beyeranlinux-green+2 :background ,beyeranlinux-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(magit-branch ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(magit-item-highlight ((,class (:background ,beyeranlinux-bg+1))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,beyeranlinux-green+1))))
   `(message-header-other ((,class (:foreground ,beyeranlinux-green))))
   `(message-header-to ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(message-header-from ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,beyeranlinux-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,beyeranlinux-green))))
   `(message-mml ((,class (:foreground ,beyeranlinux-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,beyeranlinux-orange))))
   `(mew-face-header-from ((,class (:foreground ,beyeranlinux-yellow))))
   `(mew-face-header-date ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-header-to ((,class (:foreground ,beyeranlinux-red))))
   `(mew-face-header-key ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-header-private ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-header-important ((,class (:foreground ,beyeranlinux-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,beyeranlinux-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,beyeranlinux-red))))
   `(mew-face-header-xmew ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,beyeranlinux-red))))
   `(mew-face-body-url ((,class (:foreground ,beyeranlinux-orange))))
   `(mew-face-body-comment ((,class (:foreground ,beyeranlinux-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,beyeranlinux-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,beyeranlinux-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,beyeranlinux-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,beyeranlinux-red))))
   `(mew-face-mark-review ((,class (:foreground ,beyeranlinux-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-mark-delete ((,class (:foreground ,beyeranlinux-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,beyeranlinux-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-mark-unread ((,class (:foreground ,beyeranlinux-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,beyeranlinux-green))))
   `(mew-face-eof-part ((,class (:foreground ,beyeranlinux-yellow))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,beyeranlinux-cyan :background ,beyeranlinux-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,beyeranlinux-bg :background ,beyeranlinux-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,beyeranlinux-bg :background ,beyeranlinux-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,beyeranlinux-yellow))))
   `(nav-face-button-num ((,class (:foreground ,beyeranlinux-cyan))))
   `(nav-face-dir ((,class (:foreground ,beyeranlinux-green))))
   `(nav-face-hdir ((,class (:foreground ,beyeranlinux-red))))
   `(nav-face-file ((,class (:foreground ,beyeranlinux-fg))))
   `(nav-face-hfile ((,class (:foreground ,beyeranlinux-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background nil))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,beyeranlinux-bg-1))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,beyeranlinux-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,beyeranlinux-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,beyeranlinux-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,beyeranlinux-fg :weight bold))))
   `(org-checkbox ((,class (:background ,beyeranlinux-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,beyeranlinux-blue :underline t))))
   `(org-deadline-announce ((,class (:foreground ,beyeranlinux-red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,beyeranlinux-green+3))))
   `(org-formula ((,class (:foreground ,beyeranlinux-yellow-2))))
   `(org-headline-done ((,class (:foreground ,beyeranlinux-green+3))))
   `(org-hide ((,class (:foreground ,beyeranlinux-bg-1))))
   `(org-level-1 ((,class (:foreground ,beyeranlinux-orange))))
   `(org-level-2 ((,class (:foreground ,beyeranlinux-green+1))))
   `(org-level-3 ((,class (:foreground ,beyeranlinux-blue-1))))
   `(org-level-4 ((,class (:foreground ,beyeranlinux-yellow-2))))
   `(org-level-5 ((,class (:foreground ,beyeranlinux-cyan))))
   `(org-level-6 ((,class (:foreground ,beyeranlinux-green-1))))
   `(org-level-7 ((,class (:foreground ,beyeranlinux-red-4))))
   `(org-level-8 ((,class (:foreground ,beyeranlinux-blue-4))))
   `(org-link ((,class (:foreground ,beyeranlinux-yellow-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,beyeranlinux-green+4))))
   `(org-scheduled-previously ((,class (:foreground ,beyeranlinux-red-4))))
   `(org-scheduled-today ((,class (:foreground ,beyeranlinux-blue+1))))
   `(org-special-keyword ((,class (:foreground ,beyeranlinux-yellow-1))))
   `(org-table ((,class (:foreground ,beyeranlinux-green+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,beyeranlinux-orange))))
   `(org-todo ((,class (:bold t :foreground ,beyeranlinux-red :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,beyeranlinux-red :weight bold :underline nil))))
   `(org-column ((,class (:background ,beyeranlinux-bg-1))))
   `(org-column-title ((,class (:background ,beyeranlinux-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,beyeranlinux-cyan))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,beyeranlinux-blue+1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,beyeranlinux-red+1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,beyeranlinux-orange))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,beyeranlinux-blue-1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,beyeranlinux-green+4))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,beyeranlinux-red-3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,beyeranlinux-yellow-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,beyeranlinux-green+2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,beyeranlinux-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,beyeranlinux-red-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,beyeranlinux-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,beyeranlinux-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,beyeranlinux-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,beyeranlinux-red))))
   `(rpm-spec-package-face ((,class (:foreground ,beyeranlinux-red))))
   `(rpm-spec-section-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,beyeranlinux-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,beyeranlinux-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,beyeranlinux-orange))))
   `(rst-level-2-face ((,class (:foreground ,beyeranlinux-green+1))))
   `(rst-level-3-face ((,class (:foreground ,beyeranlinux-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,beyeranlinux-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,beyeranlinux-cyan))))
   `(rst-level-6-face ((,class (:foreground ,beyeranlinux-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,beyeranlinux-red-3 :background ,beyeranlinux-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,beyeranlinux-blue-1 :background ,beyeranlinux-bg :weight bold))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,beyeranlinux-red))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,beyeranlinux-bg+1))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,beyeranlinux-bg :foreground ,beyeranlinux-bg+1))))
   `(whitespace-hspace ((,class (:background ,beyeranlinux-bg :foreground ,beyeranlinux-bg+1))))
   `(whitespace-tab ((,class (:background ,beyeranlinux-bg :foreground ,beyeranlinux-red))))
   `(whitespace-newline ((,class (:foreground ,beyeranlinux-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,beyeranlinux-red :background ,beyeranlinux-bg))))
   `(whitespace-line ((,class (:background ,beyeranlinux-bg-05 :foreground ,beyeranlinux-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,beyeranlinux-orange :foreground ,beyeranlinux-orange))))
   `(whitespace-indentation ((,class (:background ,beyeranlinux-yellow :foreground ,beyeranlinux-red))))
   `(whitespace-empty ((,class (:background ,beyeranlinux-yellow :foreground ,beyeranlinux-red))))
   `(whitespace-space-after-tab ((,class (:background ,beyeranlinux-yellow :foreground ,beyeranlinux-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,beyeranlinux-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,beyeranlinux-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,beyeranlinux-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,beyeranlinux-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,beyeranlinux-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,beyeranlinux-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,beyeranlinux-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,beyeranlinux-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,beyeranlinux-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,beyeranlinux-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,beyeranlinux-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,beyeranlinux-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,beyeranlinux-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,beyeranlinux-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,beyeranlinux-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,beyeranlinux-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,beyeranlinux-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,beyeranlinux-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,beyeranlinux-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,beyeranlinux-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,beyeranlinux-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,beyeranlinux-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,beyeranlinux-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,beyeranlinux-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,beyeranlinux-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,beyeranlinux-green+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,beyeranlinux-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,beyeranlinux-bg-1 :foreground ,beyeranlinux-bg-1))))))


(provide-theme 'beyeranlinux)
