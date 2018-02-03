(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000)))
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
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; c/c++ config
(require 'cc-mode)
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)   ;; no tabs
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; Open .h files in c++-mode
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(setq compilation-ask-about-save nil)
(defun my/compilation-popup ()
  (interactive)
  (popwin:popup-buffer "*compilation*" :stick t))

;;
;; Org mode
;;
(use-package org
  :ensure t
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

;; (use-package smex :ensure t
;;   :config
;;   (smex-initialize))
(use-package try :ensure t
  :defer t
  :config (message "Loaded try"))
(use-package general :ensure t
  :config
  ;; (general-define-key "C-'" 'avy-goto-word-1)
  )
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))
(use-package avy :ensure t
  :commands (avy-goto-word-1))
(use-package swiper :ensure t)
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
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)
  
  (evil-mode)
  )
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

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
     )
   :config
   (setq helm-ff-file-name-history-use-recentf t)
   (global-set-key (kbd "M-y") 'helm-show-kill-ring)

   ;; Make Helm open at the bottom with height=40%
   ;; From https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
   (add-to-list 'display-buffer-alist
 	       `(,(rx bos "*helm" (* not-newline) "*" eos)
 		 (display-buffer-in-side-window)
 		 (inhibit-same-window . t)
 		 (window-height . 0.3)))

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
   )
 (use-package helm-ls-git
   :ensure t
   :after helm
   :defer t   ;; TODO how to mix the delayed load of :bind with use-package?
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
  "b" '(:ignore t :which-key "buffers")
  "bb" 'helm-buffers-list
  "bk" 'kill-buffer  ; change buffer, chose using ivy
  "f" '(:ignore t :which-key "files")
  "ff" 'helm-find-files
  "fr" 'helm-recentf
  "fs" 'save-buffer
  "p" '(:ignore t :which-key "project")
  "pf" '(counsel-git :which-key "find file in git dir")        ; find file in git project
  "h" '(:ignore t :which-key "help")
  "ha" 'helm-apropos
  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gb" 'magit-blame
  "gl" 'magit-log-head
  "gL" 'magit-log
  "c" 'compile
  "w" '(:ignore t :which-key "window")
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
  ;; "bb" 'ivy-switch-buffer
  ;; "ff" 'counsel-find-file  ; find file using ivy
  ;; "/" 'counsel-git-grep   ;find string in git project
  )


 (windmove-default-keybindings)

 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (windresize windsize esup beacon evil-magit magit helm-git-grep zoom-frm smex zenburn-theme which-key use-package try org-bullets hc-zenburn-theme general evil counsel ace-window))))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

 (setq delete-old-versions -1 )          ; delete excess backup versions silently
 (setq version-control t )               ; use version control
 (setq vc-make-backup-files t )          ; make backups file even when in version controlled dir
 (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
 (setq vc-follow-symlinks t )		; don't ask for confirmation when opening symlinked file
 (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
 (setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
 (setq ring-bell-function 'ignore )	; silent bell when you make a mistake
 (setq coding-system-for-read 'utf-8 )	; use utf-8 by default
 (setq coding-system-for-write 'utf-8 )
 (setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
 (setq default-fill-column 80)		; toggle wrapping text at the 80th character
 (defalias 'yes-or-no-p 'y-or-no-p)
 ;; (setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(put 'narrow-to-region 'disabled nil)
