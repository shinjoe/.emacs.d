(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects .5)
			  (recents . 5))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
			  :prefix "SPC")
  (my-leader-def
    :states 'normal
    :keymaps 'override
    "e" 'eval-last-sexp
    "g" 'magit-status
    "f" 'projectile-find-file
    "p" 'projectile-switch-project
    "q" 'restart-emacs
    "x" 'delete-window))

(use-package evil-magit
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))


(use-package restart-emacs
  :ensure t)

(global-hl-line-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox-dark-hard)))
 '(custom-safe-themes
   (quote
    ("f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" default)))
 '(package-selected-packages
   (quote
    (evil-magit general evil magit projectile rainbow-delimiters gruvbox-theme geiser spaceline spacemacs-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :foundry "outline" :slant normal :weight normal :height 158 :width normal)))))
