
(setq custom-file "~/.emacs.d/custom.el")

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
; See: https://superuser.com/questions/941286/disable-default-option-key-binding


(setq visible-bell t)
(tool-bar-mode 0)

;; TODO: double-check that backup files are actually created
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Install and configure use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(defun ensure-installed (package)
  (unless (package-installed-p package)
    (package-install package)))
(ensure-installed 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ace-window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; crux
(use-package crux
  :bind (("C-c o" . crux-open-with)))

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package all-the-icons)
(require 'all-the-icons)

(use-package eglot)

;; FFAP
(global-set-key (kbd "C-c .") 'ffap)

(use-package git-auto-commit-mode)

;; golden-ratio-scroll-screen
(use-package golden-ratio-scroll-screen)
(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; ivy/swiper/counsel
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode))
(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))
(use-package counsel
  :config
  (global-set-key (kbd "C-c m") 'counsel-imenu)
  (global-set-key (kbd "M-x") 'counsel-M-x))

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(use-package restart-emacs)
(global-set-key (kbd "C-c r") 'restart-emacs)

(use-package magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

(use-package projectile
  :init
  (setq projectile-project-search-path
	'(("~/workplace/black-mamba-management" . 2)
	  "~/Library/CloudStorage/WorkDocsDrive-Documents")
	projectile-switch-project-action #'projectile-vc)
  :config
  (projectile-discover-projects-in-search-path)
  :bind-keymap 
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map))
  :bind
  ("C-c s" . projectile-ripgrep))

;; which-key
(use-package which-key)
(require 'which-key)
(which-key-mode)
