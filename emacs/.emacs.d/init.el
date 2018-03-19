(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000
                                     gc-cons-percentage .1)))
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                                          gcs-done)))

;; (require 'package)
;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil 
      package-user-dir "~/.emacs.d/elpa/")
(setq package-archives '(("melpa"     . "https://melpa.org/packages/")
			 ("org"       . "http://orgmode.org/elpa/")
			 ("gnu"      . "http://elpa.gnu.org/packages/")
			 ))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

;; Disable toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(use-package windsize
  :ensure t
  :config
  (setq windsize-cols 2
        windsize-rows 1)
  (global-set-key (kbd "<C-up>") 'windsize-up)
  (global-set-key (kbd "<C-down>") 'windsize-down)
  (global-set-key (kbd "<C-left>") 'windsize-left)
  (global-set-key (kbd "<C-right>") 'windsize-right)
  )


 (windmove-default-keybindings)
 (xterm-mouse-mode)                     ; Enable the mouse in the terminal

;; c/c++ config
(use-package cc-mode
  :defer t
  :config
    (setq c-default-style "linux"
        c-basic-offset 4)
    (setq-default indent-tabs-mode nil)   ;; no tabs
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; Open .h files in c++-mode
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
    (setq compilation-ask-about-save nil)
    (defun my/compilation-popup ()
      (interactive)
      (popwin:popup-buffer "*compilation*" :stick t :height 30))
)

;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   )
;; (use-package company
;;   :ensure t)
;; (use-package company-lsp
;;   :after lsp-mode
;;   :ensure t
;;   :config
;;   (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
;;   )

(use-package flycheck
  :ensure t
  :after lsp-mode
  )
(use-package markdown-mode             ; Apparently required by lsp-ui
  :ensure t
  :defer)
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   )

;;   :ensure t
;;   :after evil
;;   :config
;;   ;; Arch Linux aur/cquery-git aur/cquery
;;   (setq cquery-executable "/usr/bin/cquery"
;;   ;; Log file
;;   cquery-extra-args '("--log-file=/tmp/cq.log"))
;;   ;; Initialization options
;;   cquery-extra-init-params '()
;;   (evil-define-key 'normal c-mode-map (kbd "M-.") 'xref-find-definitions)
;;   (add-to-list 'xref-prompt-for-identifier 'xref-find-references t)
;;   (defun cquery//enable ()
;;     (when
;;         (and buffer-file-name
;;              (or (locate-dominating-file default-directory "compile_commands.json")
;;                  (locate-dominating-file default-directory ".cquery")))
;;       (lsp-cquery-enable)))
;;   (add-hook 'c-mode-common-hook #'cquery//enable)
;;   )

;; (use-package helm-xref
;;   :ensure t
;;   :config
;;   (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
;;   )


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )
(use-package auto-yasnippet :ensure t)


(use-package elpy
  :defer
  :ensure t
  :config
  (elpy-enable)
  )

;;
;; Org mode
;;
(use-package org
  :ensure t
  :defer t
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files (list "~/todo.org"
                               "~/foo.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-log-done t)
  (setq org-default-notes-file "~/notes.org")
  (define-key global-map "\C-cc" 'org-capture)
  )


(use-package popwin :ensure t
  :config
  (popwin-mode 1)
  )

(use-package try :ensure t :defer t)
(use-package general :ensure t)
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (global-hl-line-mode))


(use-package avy :ensure t
  :commands (avy-goto-word-1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.3
        which-key-sort-order 'which-key-description-order ; sorty by description. Keeps related commands together
        which-key-max-display-columns 7
        )
  )
(use-package evil
  :ensure t
  :defer 0.02
  :config
  ;; Allow TAB to indent code in Normal mode
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "TAB") nil))
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "0")       'evil-digit-argument-or-evil-beginning-of-line
    (kbd "^")       'evil-first-non-blank
    (kbd "$")       'evil-end-of-line
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

 (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)) 
  (evil-mode)
  )

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 5.0)
  )


(use-package hydra :ensure t)

(use-package magit
  :ensure t
  :defer t
  )
(use-package evil-magit
  :ensure t
  :after magit
  )

 (use-package helm
   :ensure t
   :diminish helm-mode
   :bind
    (("C-x C-f" . helm-find-files)
     ("C-x b" . helm-buffers-list)
     ("M-x" . helm-M-x)
     ("M-s s" . helm-occur)
     )
   :config
   (setq helm-ff-file-name-history-use-recentf t
         helm-buffer-max-length 40)     ; make file name column wider
   (global-set-key (kbd "M-y") 'helm-show-kill-ring)

   ;; Make Helm open at the bottom with height=40%
   ;; From https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
   (add-to-list 'display-buffer-alist
 	       `(,(rx bos "*helm" (* not-newline) "*" eos)
 		 (display-buffer-in-side-window)
 		 (inhibit-same-window . t)
 		 (window-height . 0.4)))

   (helm-mode 1)

   (define-key helm-map "\t" 'helm-execute-persistent-action)
   (define-key helm-map (kbd "<backtab>") 'helm-select-action)
   (define-key helm-map (kbd "C-j") 'helm-next-line)
   (define-key helm-map (kbd "C-k") 'helm-previous-line)
   (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
   ;; (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
   )
 (use-package helm-ag :ensure t
   :after helm
   :config
   (setq helm-ag-insert-at-point 'symbol
         helm-ag-use-agignore t)
   )
 (use-package helm-ls-git
   :ensure t
   :after helm
   :defer t   ;; TODO how to mix the delayed load of :bind with use-package?
  ) 
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package golden-ratio
  :ensure t
  :defer 1
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  ace-window)))
  ;; (golden-ratio-mode 1)
  )

 (defun xah-comment-dwim ()
   "Like `comment-dwim', but toggle comment if cursor is not at end of line.

 URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
 Version 2016-10-25"
   (interactive)
   (if (region-active-p)
       (comment-dwim nil)
     (let (($lbp (line-beginning-position))
           ($lep (line-end-position)))
       (if (eq $lbp $lep)
           (progn
             (comment-dwim nil))
         (if (eq (point) $lep)
             (progn
               (comment-dwim nil))
           (progn
             (comment-or-uncomment-region $lbp $lep)
             ))))))


(defun my/toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
                 (delete-other-windows))))


 (general-define-key
  ;; replace default keybindings
  ;; "C-s" 'swiper
  "M-;" 'xah-comment-dwim
  ;; "M-x" 'counsel-M-x

  )

 (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  "/" 'helm-do-ag
  "?" 'helm-do-ag-project-root

  "x" '(:ignore t :which-key "text")
  "xa" 'align-regexp

  "b" '(:ignore t :which-key "buffers/bookmarks")
  "bb" 'helm-buffers-list
  "bk" 'kill-buffer  ; change buffer, chose using ivy
  "bm" 'helm-bookmarks

  "f" '(:ignore t :which-key "files")
  "ff" 'helm-find-files
  "fr" 'helm-recentf
  "fo" 'ff-find-other-file
  "fs" 'save-buffer

  "s" '(:ignore t :which-key "snippets")
  "sc" 'aya-create
  "se" 'aya-expand

  "p" '(:ignore t :which-key "project")
  "pf" '(counsel-git :which-key "find file in git dir")        ; find file in git project

  "h" '(:ignore t :which-key "help")
  "ha" 'helm-apropos
  "hr" 'helm-resume

  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gb" 'magit-blame
  "gl" 'magit-log-head
  "gL" 'magit-log

  "c" 'compile

  "w" '(:ignore t :which-key "window")
  "wo" 'ace-window
  "wm" 'my/toggle-maximize-buffer
  "wc" 'delete-window
  "wd" 'delete-window
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  "wh" 'evil-window-left
  "wl" 'evil-window-right

  "SPC" 'avy-goto-word-1
  "."  'helm-mini
  "," 'helm-browse-project
  "'" 'my/compilation-popup
  )



 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-lsp-async t)
 '(company-lsp-cache-candidates nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("12b204c8fcce23885ce58e1031a137c5a14461c6c7e1db81998222f8908006af" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" default)))
 '(fci-rule-color "#383838")
 '(lsp-highlight-symbol-at-point nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (solarized-theme yasnippet elpy company-lsp company markdown-mode lsp-ui helm-xref cquery lsp-mode evil-surround windresize windsize esup beacon evil-magit magit helm-git-grep zoom-frm smex zenburn-theme which-key use-package try org-bullets hc-zenburn-theme general evil counsel ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(hl-line ((t (:background "#525252" :weight bold))))
 '(lsp-face-highlight-textual ((t (:background "DarkGoldenrod3")))))

 (setq delete-old-versions -1 )          ; delete excess backup versions silently
 (setq version-control t )               ; use version control
 (setq vc-make-backup-files t )          ; make backups file even when in version controlled dir
 (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
 (setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file
 (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
 (setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
 (setq ring-bell-function 'ignore )	; silent bell when you make a mistake
 (setq coding-system-for-read 'utf-8 )	; use utf-8 by default
 (setq coding-system-for-write 'utf-8 )
 (setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
 (setq default-fill-column 80)		; toggle wrapping text at the 80th character
 (defalias 'yes-or-no-p 'y-or-n-p)
 ;; (setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(put 'narrow-to-region 'disabled nil)
