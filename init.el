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


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 5)
                          (recents . 5))))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  (counsel-mode)
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (add-to-list 'evil-buffer-regexps '("*Backtrace*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Compile-Log*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Help*" . normal))
  (add-to-list 'evil-buffer-regexps '("* Racket REPL *" . normal))
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal)))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :prefix "SPC")
  (my-leader-def
    :states 'normal
    :keymaps 'override
    "SPC" 'execute-extended-command
    "TAB" 'switch-to-buffer
    "e" 'eval-last-sexp
    "j" '(lambda()(interactive)(split-window-below)(other-window 1)(switch-to-buffer (get-buffer-create "*scratch*")))
    "k" '(lambda()(interactive)(split-window-right)(other-window 1)(switch-to-buffer (get-buffer-create "*scratch*")))
    "g" 'magit-status
    "f" 'projectile-find-file
    "F" 'find-file
    "p" 'projectile-switch-project
    "s" 'ace-window
    "q" 'restart-emacs
    "x" 'delete-window
    "y" '(lambda()(interactive)(let ((current-prefix-arg 4))(call-interactively 'geiser-compile-current-buffer)))
    "z" '(lambda()(interactive)(find-file "~/.emacs.d/init.el"))))

(use-package evil-magit
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

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
  :config
  (add-hook 'geiser-autodoc-mode-hook  '(lambda()(diminish 'geiser-autodoc-mode)))
  (setq geiser-active-implementations '(racket))
  (setq geiser-racket-binary "Racket.exe"))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

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
  :hook (after-init . global-company-mode))


(use-package restart-emacs
  :ensure t)

(global-prettify-symbols-mode 1)
(global-hl-line-mode 1)
(setq help-window-select t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 3)
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face tabs spaces empty trailing space-before-tab newline indentation space-after-tab space-mark tab-mark))
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'whitespace-mode-hook '(lambda()(diminish 'whitespace-mode)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'with-editor-mode-hook 'evil-insert-state)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" default)))
 '(package-selected-packages
   (quote
    (doom-themes beacon counsel diminish smartparens evil-magit general evil magit projectile rainbow-delimiters geiser spaceline spacemacs-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :foundry "outline" :slant normal :weight normal :height 158 :width normal))))
 '(whitespace-space ((t (:background "nil" :foreground "gray25")))))
