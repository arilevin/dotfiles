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
			 ;; ("gnu"      . "http://elpa.gnu.org/packages/")
			 ))





(package-initialize)

;;; Rtags support
;; (setq package-directory-list '("/usr/local/share/emacs/site-lisp/rtags/"))
(use-package company :ensure t)
(use-package company-rtags
  :load-path "/usr/local/share/emacs/site-lisp/rtags"
  :config
  (setq rtags-completions-enabled t)
  (setq company-async-timeout 4)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<backtab>") (function company-complete))
  )

(use-package rtags
  :load-path "/usr/local/share/emacs/site-lisp/rtags"
  ;; :init
  ;; (add-hook 'c-mode-common-hook #'flycheck-mode)
  :config
  (setq rtags-display-result-backend 'helm)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)

  (rtags-enable-standard-keybindings)
  ;; More keybindings under general-define-key because otherwise
  ;; evil mode was shadowing them
  )

;; (use-package flycheck :ensure t :defer t
;;   :init
;;   (add-hook 'c-mode-common-hook #'flycheck-mode)
;;   )

;; (use-package flycheck-rtags
;;   :load-path "/usr/local/share/emacs/site-lisp/rtags"
;;   :config
;;   (defun my-flycheck-rtags-setup ()
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil) ;; use rtags overlays
;;     (setq-local flycheck-check-syntax-automatically nil))
;;   ;; c-mode-common-hook is also called by c++-mode
;;   (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
;;   )


;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")
;; (require 'rtags)
;; (require 'company)
;; (require 'company-rtags)


;; (setq rtags-display-result-backend 'helm)
;; (setq rtags-autostart-diagnostics t)
;; (setq rtags-completions-enabled t)
;; (push 'company-rtags company-backends)

;; (rtags-diagnostics)
;; (global-company-mode)
;; (rtags-enable-standard-keybindings)

;; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;  (flycheck-select-checker 'rtags)
;;  (setq-local flycheck-highlighting-mode nil) ;; use rtags overlays
;;  (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;;; end rtags


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

;; Disable toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

; Highlight matching parens
(show-paren-mode)


(eval-after-load "tramp"
  (lambda ()
       (setq tramp-remote-path '("/usr/local/bin" "/usr/local/sbin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin"  "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")))
    ;; (add-to-list 'tramp-remote-path "/usr/local/bin" ))
  )

(column-number-mode)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 10000)
(setq scroll-margin 0)
(setq bookmark-save-flag 1)  ;; save bookmarks after every operation

;; (setq shell-command-switch "-ic") ;; Run shell interactively to source .bashrc


(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

(use-package symbol-overlay
  :ensure t
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  )

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

(use-package transpose-frame
  :ensure t)



;; c/c++ config
(use-package cc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; Open .h files in c++-mode
  :config
    (setq c-default-style "linux"
        c-basic-offset 4)
    (setq-default indent-tabs-mode nil)   ;; no tabs
    (setq-default show-trailing-whitespace t)
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
    (setq compilation-ask-about-save nil)
    (setq compilation-scroll-output 'first-error)
    (setq compilation-skip-threshold 2)
    (defun my/compilation-popup ()
      (interactive)
      (popwin:popup-buffer "*compilation*" :stick t :height 35))

    (defun my/compile-in-toplevel ()
      (interactive)
      (let ((default-directory (vc-root-dir)))
        (call-interactively #'compile)))

    ;; Make ff-find-other-file work for my file types
    (setq-default ff-other-file-alist
                  '(
                    ("\\.*_inl\\.h\\'" (".h"))
                    ("\\.h\\'" ("_inl.h" ".cpp" ".c"))
                    ("\\.cpp\\'" (".h"))
                    ("\\.c\\'" (".h"))
                    )
                  )
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


(use-package visual-regexp :ensure t)
(use-package visual-regexp-steroids :ensure t)


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



(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
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
(defun my/tasks-popup ()
  (interactive)
  (let (height popwin:popup-window-height)
    (setq popwin:popup-window-height 30)
    (popwin:find-file "~/org/tasks.org")
    (setq popwin:popup-window-height height)
    )
  )

(use-package org
  :ensure t
  :defer t
  :init
  (define-key global-map "\C-cc" 'org-capture)
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-agenda-files (list "~/org/tasks.org"
                               "~/org/notes.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-log-done t)
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-capture-templates
	'(("t" "Task" entry (file "~/org/tasks.org") "* TODO %?\n %i\n %a")
	  ("n" "Note" entry (file "~/org/notes.org") "* %?" )
          ("l" "Log work" entry (file "~/org/tasks.org") "* %T: %?  [[%l][(link)]]")
	  ))
  (add-hook 'org-capture-mode-hook 'evil-insert-state) ; capture templates drop you right into insert mode
  (setq org-M-RET-may-split-line nil)  ;; makes it so you can hit M-RET anywhere in the line to split
  (setq org-startup-indented t)
  (setq org-ellipsis " ▶")

  ;; Refile settings
  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-refile-targets '((nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)  ; defaults to t and helm can't do the incremental completion
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-insert-heading-respect-content t) ; insert headings after subtree. Preserves folds


  (add-hook 'org-insert-heading-hook 'evil-insert-state)
  )


(use-package popwin :ensure t
  :config
  (push '(help-mode :height 30) popwin:special-display-config)
  (push '(compilation-mode :height 35 :stick t) popwin:special-display-config)
  (popwin-mode 1)
  )

(use-package try :ensure t :defer t)
(use-package general :ensure t)
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  (global-hl-line-mode)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(mode-line-inactive
       ((t (:foreground ,zenburn-green-1
  			:background ,zenburn-bg+05
  			:box (:line-width -1 :style released-button)))))

     '(hl-line ((t (:background "#525252"))))
     ))
  )


(use-package avy :ensure t
  :commands (avy-goto-word-1
             avy-goto-char-timer)
  )

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
  :init

  (setq evil-default-cursor (quote (t "#750000"))
        evil-visual-state-cursor '("#880000" box)
        evil-normal-state-cursor '("#750000" box)
        evil-insert-state-cursor '("#e2e222" box)
        )
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

(use-package evil-escape :ensure t
  :after evil
  :config
  (evil-escape-mode)
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

(use-package diminish
  :ensure t
  )

;; These two are built in but I'm using use-package because I couldn't figure
;; out how else to diminish them
(use-package undo-tree
  :diminish undo-tree-mode
  )
(use-package abbrev
  :diminish abbrev-mode
  )

 (use-package helm
   :ensure t
   :diminish helm-mode
   :commands (describe-mode describe-variable)
   :bind
    (("C-x C-f" . helm-find-files)
     ("C-x b" . helm-buffers-list)
     ("M-x" . helm-M-x)
     ("M-s" . helm-occur)
     )
   :config
   (setq helm-ff-file-name-history-use-recentf t
         helm-buffer-max-length 40)     ; make file name column wider
   (global-set-key (kbd "M-y") 'helm-show-kill-ring)

   ;; Make Helm open at the bottom with height=40%
   ;; From https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
   ;; (add-to-list 'display-buffer-alist
   ;; 	       `(,(rx bos "*helm" (* not-newline) "*" eos)
   ;; 		 (display-buffer-in-side-window)
   ;; 		 (inhibit-same-window . t)
   ;; 		 (window-height . 0.4)))

   (helm-mode 1)

   (define-key helm-map "\t" 'helm-execute-persistent-action)
   (define-key helm-map (kbd "<backtab>") 'helm-select-action)
   (define-key helm-map (kbd "C-j") 'helm-next-line)
   (define-key helm-map (kbd "C-k") 'helm-previous-line)
   (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)
   ;; (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)

   ;; XXX experiment
   (setq helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

	 helm-echo-input-in-header-line t
	 helm-display-header-line t) ;; input close to where I type


   ;; This doesn't seem to work in the terminal
   ;; (defun helm-toggle-header-line ()
   ;;   (if (= (length helm-sources) 1)
   ;; 	 (set-face-attribute 'helm-source-header nil :height 0.5)
   ;;     (set-face-attribute 'helm-source-header nil :height 1.0)))

   ;; (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)


   ;; (defun spacemacs//helm-hide-minibuffer-maybe ()
   ;;   "Hide minibuffer in Helm session if we use the header line as input field."
   ;;   (when (with-helm-buffer helm-echo-input-in-header-line)
   ;;     (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
   ;;       (overlay-put ov 'window (selected-window))
   ;;       (overlay-put ov 'face
   ;;      	      (let ((bg-color (face-background 'default nil)))
   ;;      		`(:background ,bg-color :foreground ,bg-color)))
   ;;       (setq-local cursor-type nil))))

   ;; (add-hook 'helm-minibuffer-set-up-hook
   ;;           'spacemacs//helm-hide-minibuffer-maybe)

   ;; (setq helm-autoresize-max-height 30)
   ;; (setq helm-autoresize-min-height 20)
   ;; (helm-autoresize-mode 1)

   )

 (use-package ag :ensure t)
 (use-package helm-ag :ensure t
   :after helm
   :config
   (setq helm-ag-insert-at-point 'symbol
         helm-ag-use-agignore t)
   (define-key helm-ag-mode-map (kbd "<tab>") 'helm-ag-mode-jump-other-window)
   (define-key helm-ag-mode-map (kbd "<ret") 'helm-ag-mode-jump-other-window)
   )
 (use-package helm-ls-git
   :ensure t
   :after helm
   :defer t   ;; TODO how to mix the delayed load of :bind with use-package?
   :config
   ;; include submodules in file listing. --recurse-submodules needs newer git
   (setq helm-ls-git-ls-switches '("ls-files" "--full-name" "--recurse-submodules" "--"))
  )
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package protobuf-mode
  :ensure t)

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
  :keymaps 'override
  :non-normal-prefix "C-SPC"
  "/" 'helm-do-ag
  "?" 'helm-do-ag-project-root

  "x" '(:ignore t :which-key "text")
  "xa" 'align-regexp

  "a" '(:ignore t :which-key "ag")
  "a." 'ag-project-at-point
  "ar" 'ag-regexp
  "ap" 'ag-project-regexp

  "b" '(:ignore t :which-key "buffers/bookmarks")
  "bb" 'helm-buffers-list
  "bk" 'kill-buffer  ; change buffer, chose using ivy
  "bm" 'helm-bookmarks
  "bc" '(clone-indirect-buffer-other-window :which-key "clone indirect")

  "f" '(:ignore t :which-key "files")
  "ff" 'helm-find-files
  "fr" 'helm-recentf
  "fo" 'ff-find-other-file
  "fs" 'save-buffer
  "fi" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "init.el")

  "t" 'my/tasks-popup

  "r" '(:ignore t :which-key "regexp search")
  "r/" 'vr/isearch-forward
  "r?" 'vr/isearch-backward
  "rq" 'vr/query-replace

  "s" '(:ignore t :which-key "snippets|search")
  "sc" 'aya-create
  "se" 'aya-expand

  "p" '(:ignore t :which-key "project")
  "pf" '(counsel-git :which-key "find file in git dir")        ; find file in git project

  "h" '(:ignore t :which-key "helm")
  "ha" 'helm-apropos
  "hr" 'helm-resume

  "l" '(:ignore t :whichk-key "highlight")
  "lr" 'highlight-regexp
  "l." 'highlight-symbol-at-point
  "lu" 'unhighlight-regexp

  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gb" 'magit-blame
  "gl" 'magit-log-head
  "gL" 'magit-log

  "c" 'my/compile-in-toplevel

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
  "wt" 'transpose-frame
  "wf" 'flip-frame
  "wF" 'flop-frame
  "wr" 'rotate-frame-clockwise
  "wR" 'rotate-frame-anticlockwise

  "SPC" 'avy-goto-char-timer
  "SPC" 'avy-goto-word-1
  "."  'helm-mini
  "," 'helm-browse-project
  "'" 'my/compilation-popup
  )

(general-define-key
 :states '(normal visual insert emacs)
 ;; :keymaps 'c++-mode-base-map
  "M-." 'rtags-find-symbol-at-point
  "M-," 'rtags-find-references-at-point
  "M->" 'rtags-find-symbol
  "M-<" 'rtags-find-references
  "M-?" 'rtags-display-summary
  "M-{" 'rtags-location-stack-back
  "M-}" 'rtags-location-stack-forward
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
 '(fci-rule-color "#383838")
 '(helm-source-names-using-follow
   (quote
    ("Search at ~/debesys/orders/sgx_titan/include/sgx_titan/" "Occur")))
 '(lsp-highlight-symbol-at-point nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/org/notes.org")))
 '(package-selected-packages
   (quote
    (evil-goggles w3m treemacs evil-escape protobuf-mode evil-terminal-cursor-changer symbol-overlay solarized-theme yasnippet elpy company-lsp company markdown-mode lsp-ui helm-xref cquery lsp-mode evil-surround windresize windsize esup beacon evil-magit magit helm-git-grep zoom-frm smex zenburn-theme which-key use-package try org-bullets hc-zenburn-theme general evil counsel ace-window)))
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
 '(avy-lead-face ((t (:background "#3F3F3F" :foreground "color-81" :inverse-video nil :weight bold))))
 '(avy-lead-face-0 ((t (:background "#3F3F3F" :foreground "color-220" :inverse-video nil :weight bold))))
 '(avy-lead-face-1 ((t (:background "#3F3F3F" :foreground "green" :inverse-video nil :weight bold))))
 '(avy-lead-face-2 ((t (:background "#3F3F3F" :foreground "#DCA3A3" :inverse-video nil :weight bold))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(lsp-face-highlight-textual ((t (:background "DarkGoldenrod3"))))
 '(which-func ((t (:foreground "yellow")))))

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

(put 'narrow-to-region 'disabled nil)
