;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/modes/"))

(use-package sj-helpers
  :commands (sj-create-minor-mode sj-delete sj-toggle-comments))
(use-package pop-pretty
  :hook (python-mode . pop-pretty-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :hook (prog-mode . beacon-mode))

(use-package flycheck
  :ensure t
  :defer t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))
(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package counsel
  :ensure t
  :hook (after-init . counsel-mode)
  :diminish counsel-mode
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel-projectile
  :ensure t
  :hook (after-init . counsel-projectile-mode))

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (add-to-list 'evil-buffer-regexps '("*Backtrace*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Compile-Log*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Help*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Cargo Build*" . normal))
  (add-to-list 'evil-buffer-regexps '("* Racket REPL *" . normal))
  (add-to-list 'evil-buffer-regexps '("*Customize Group: Emacs*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal)))

(use-package general
  :ensure t
  :config
  (general-define-key
   :states 'insert
   :keymaps 'prog-mode-map
   "C-n" '(lambda()(interactive)(if (company-tooltip-visible-p)(company-select-next)(company-manual-begin)))
   "C-p" 'company-select-previous
   "C-<backspace>" 'sj-delete)
  (general-define-key
   :states 'normal
   :keymaps '(help-mode-map messages-buffer-mode-map debugger-mode-map)
   "q" 'quit-window)
  (general-define-key
   :states 'motion
   :keymaps 'org-mode-map
   "t" 'org-todo)
  (general-create-definer my-leader-def
    :prefix "SPC")
  (my-leader-def
    :states '(normal visual)
    :keymaps 'override
    "SPC" 'execute-extended-command
    "TAB" 'dired-sidebar-toggle-sidebar
    "e" 'eval-last-sexp
    "j" '((lambda()(interactive)(split-window-below)(other-window 1)(switch-to-buffer (get-buffer-create "*scratch*")))
          :which-key "split-below")
    "k" '((lambda()(interactive)(split-window-right)(other-window 1)(switch-to-buffer (get-buffer-create "*scratch*")))
          :which-key "split-right")
    "K" '((lambda()(interactive)(split-window-right)(other-window 1)(eshell))
          :which-key "split-right-term")
    "l" 'switch-to-buffer
    "g" 'magit-status
    "f" 'projectile-find-file
    "F" 'find-file
    "O" '((lambda()(interactive)(find-file "~/.emacs.d/org/todo.org"))
          :which-key "org TODO")
    "p" 'projectile-switch-project
    "s" 'ace-window
    "q" 'restart-emacs
    "x" 'delete-window
    "y" '((lambda()(interactive)(let ((current-prefix-arg 4))(call-interactively 'geiser-compile-current-buffer)))
          :which-key "geiser compile")
    "z" '((lambda()(interactive)(find-file "~/.emacs.d/init.el"))
          :which-key "init.el")))

(use-package evil-magit
  :ensure t
  :after magit)

(use-package rg
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-city-lights t))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package geiser
  :ensure t
  :defer t
  :config
  (add-hook 'geiser-autodoc-mode-hook '(lambda()(diminish 'geiser-autodoc-mode)))
  (setq geiser-active-implementations '(racket))
  (setq geiser-racket-binary "Racket.exe"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-init))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode))

(use-package diminish
  :ensure t
  :config
  (diminish 'undo-tree-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 2))

(use-package restart-emacs
  :ensure t
  :defer t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defconst python--prettify-symbols-alist
    '(("lambda"  . ?λ)))

(global-set-key "\C-k" 'sj-toggle-comments)
(global-set-key "\C-s" 'swiper)
(global-prettify-symbols-mode 1)
(global-hl-line-mode 1)
(setq ring-bell-function 'ignore)
(setq help-window-select t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq display-line-numbers-width-start t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face tabs spaces empty trailing space-before-tab newline indentation space-after-tab space-mark tab-mark))
(setq org-agenda-files '("~/.emacs.d/org/"))
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'whitespace-mode-hook '(lambda()(diminish 'whitespace-mode)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'with-editor-mode-hook 'evil-insert-state)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'emacs-startup-hook
          '(lambda()(setq gc-cons-threshold 16777216
                     gc-cons-percentage 0.1)))

(defvar my-prev-whitespace-mode nil)
(make-variable-buffer-local 'my-prev-whitespace-mode)
(defun pre-popup-draw ()
  "Turn off whitespace mode before showing company complete tooltip"
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1)
        (setq my-prev-whitespace-mode t))))
(defun post-popup-draw ()
  "Restore previous whitespace mode after showing company tooltip"
  (if my-prev-whitespace-mode
      (progn
        (whitespace-mode 1)
        (setq my-prev-whitespace-mode nil))))
(advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
(advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode smex flycheck elpy doom-themes beacon counsel diminish smartparens evil-magit general evil magit projectile rainbow-delimiters geiser spaceline use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :foundry "outline" :slant normal :weight normal :height 158 :width normal))))
 '(whitespace-space ((t (:background nil :foreground "gray25")))))
