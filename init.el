;; --- PRELUDE ---

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

(add-to-list 'load-path "~/.emacs.d/lisp")


;; --- USER SETUP ---

;; ace-window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; asdf
(require 'asdf)
(asdf-enable)

;; crux
(use-package crux
  :bind (("C-c o" . crux-open-with)))

;; dart
(use-package dart-mode)

;; dead-keys
(setq ns-right-alternate-modifier 'none)

;; default indentation
(setq-default indent-tabs-mode nil)

;; dired
(setq dired-hide-details-mode t)

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package all-the-icons)
(require 'all-the-icons)

;; eglot
(use-package eglot
  :hook ((java-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :init
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(java-mode . ("jdtls" "--jvm-arg=-javaagent:/Volumes/brazil-pkg-cache/packages/Lombok/Lombok-1.18.x.22025.0/AL2_x86_64/DEV.STD.PTHREAD/build/lib/lombok-1.18.24.jar" "--jvm-arg=-Xbootclasspath/a:/Volumes/brazil-pkg-cache/packages/Lombok/Lombok-1.18.x.22025.0/AL2_x86_64/DEV.STD.PTHREAD/build/lib/lombok-1.18.24.jar"
                              :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t )))))
  :bind (("C-c a" . eglot-code-actions)))

;; Emacs defaults
(setq custom-file "~/.emacs.d/custom.el")
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
; See: https://superuser.com/questions/941286/disable-default-option-key-binding
(setq column-number-mode t)
(setq visible-bell t)
(tool-bar-mode 0)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; FFAP
(global-set-key (kbd "C-c .") 'ffap)

;; fonts
(require 'init-fonts)

;; git auto-commit
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
  :bind
  (("C-c s" . counsel-rg)
   ("C-s" . swiper)))
(use-package counsel
  :bind
  ("C-c m" . counsel-imenu)
  :config
  (counsel-mode)
  :demand t)

; https://timmydouglas.com/2020/12/17/eshell-counsel.html
(defun timmy/counsel-eshell-history-action (cmd)
  "Insert cmd into the buffer"
  (interactive)
  (insert cmd))
(defun timmy/counsel-eshell-history (&optional initial-input)
  "Find command from eshell history.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
    (ivy-read "Find cmd: " (timmy/eshell-history-list)
              :initial-input initial-input
              :action #'timmy/counsel-eshell-history-action
              :caller 'timmy/counsel-eshell-history))
(defun timmy/eshell-history-list ()
  "return the eshell history as a list"
  (and (or (not (ring-p eshell-history-ring))
	   (ring-empty-p eshell-history-ring))
       (error "No history"))
  (let* ((index (1- (ring-length eshell-history-ring)))
	 (ref (- (ring-length eshell-history-ring) index))
	 (items (list)))
    (while (>= index 0)
      (setq items (cons (format "%s" (eshell-get-history index)) items)
	    index (1- index)
	    ref (1+ ref)))
    items))

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
	      ("C-r" . timmy/counsel-eshell-history)))

;; javascript
(setq js-indent-level 2)
(setq-default js2-basic-offset 2)

;; jq-mode
(use-package jq-mode
  :config
  (add-to-list 'load-path "/path/to/jq-mode-dir")
  (autoload 'jq-mode "jq-mode.el"
    "Major mode for editing jq files" t)
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
  (with-eval-after-load "json-mode"
    (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively)))

;; kill without adding to kill-ring

; Taken from https://emacs.stackexchange.com/a/22267
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (let ((cur (point))
	(eol (progn (end-of-line 1) (point))))
    (if (equal cur eol)
	(delete-char 1)
      (delete-region cur eol))))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

; bind them to emacs's default shortcut keys:
(global-set-key (kbd "C-S-k") 'my-delete-line-backward)
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; multiple-cursors
(use-package multiple-cursors
  :bind
   (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

;; open externally
(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) )))

; see http://blog.binchen.org/posts/open-url-in-emacs-with-external-browser/
(setq browse-url-generic-program
      (cond
       ((eq system-type 'darwin) "open")
       (linux (executable-find "firefox"))
       ))
(defun w3mext-open-link-or-image-or-url ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (let (url)
    (if (string= major-mode "w3m-mode")
        (setq url (or (w3m-anchor) (w3m-image) w3m-current-url)))
    (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))
    ))
(global-set-key (kbd "C-c b") 'w3mext-open-link-or-image-or-url)

;; org-mode
(org-babel-do-load-languages
'org-babel-load-languages
'((shell . t)))

(use-package ob-mermaid)

; Don't ask confirmation to evaluate some code blocks
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "mermaid")))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

; Live refresh inline images
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package ox-gfm)
(require 'ox-slack)

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

; Code to copy links out of org-mode
; See https://emacs.stackexchange.com/a/3990
(defun org-extract-link-url (text)
  (string-match org-bracket-link-regexp text)
  (substring text (match-beginning 1) (match-end 1)))

(defun my-org-retrieve-url-from-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max)))))
	 (url (org-extract-link-url text)))
    (if (not url)
        (error "Not in org link")
      (kill-new url))))

(require 'org-drawio)

(defun my-smarter-kill-ring-save ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))

(global-set-key (kbd "M-w") 'my-smarter-kill-ring-save)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(use-package restart-emacs)
(global-set-key (kbd "C-c r") 'restart-emacs)

(use-package magit)
;(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))

(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

;; prettier-js
(use-package prettier-js)

;; project.el - projectile
(use-package projectile
  :demand t
  :init
  ;; (setq projectile-project-search-path
  ;; 	'(("~/workplace" . 3)
  ;; 	  "~/Library/CloudStorage/WorkDocsDrive-Documents")
  (setq projectile-switch-project-action #'projectile-vc)
  :config
  (projectile-load-known-projects)
  :bind-keymap 
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map)))
(global-set-key (kbd "C-'") 'projectile-run-eshell)

(defun project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(add-hook 'project-find-functions #'project-override)

;; scala
(use-package scala-mode)

;; shell-pop
(use-package shell-pop
  :init
  (setq shell-pop-term-shell "eshell")
  (setq shell-pop-shell-type '("eshell" "eshell" (lambda () (eshell))))
  :bind
  (("C-t" . shell-pop)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-t") #'shell-pop)))

;; smartparens
(use-package smartparens)

;; typescript-mode
(use-package typescript-mode)

(defun typescript-settings-fn ()
  (setq typescript-indent-level 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'typescript-mode-hook 'typescript-settings-fn)

;; which-key
(use-package which-key)
(require 'which-key)
(which-key-mode)

;; whitespace-mode
(global-set-key (kbd "C-c w") #'whitespace-mode)

;; winner-mode
(winner-mode)
