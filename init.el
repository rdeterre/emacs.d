;; --- elpaca prelude
;; Bootstraps elpaca and sets up the use-package integration

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)


;; --- Standard Emacs prelude
;; Changes values of some default Emacs variables
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      column-number-mode t
      custom-file "~/.emacs.d/custom.el"
      gc-cons-threshold 80000000
      highlight-nonselected-windows t
      initial-major-mode 'org-mode
      visible-bell t)
(setq-default fill-column 120
              indent-tabs-mode nil
              tab-width 2)
(tool-bar-mode -1)

;; --- ace-window
(use-package ace-window
  :bind (("M-o" . ace-window)
         ("M-1" . goto-window-1)
         ("M-2" . goto-window-2)
         ("M-3" . goto-window-3)
         ("M-4" . goto-window-4)
         ("M-5" . goto-window-5)
         ("M-6" . goto-window-6))
  :config
  (defun goto-window-1 ()
    (interactive)
    (aw-switch-to-window (nth 0 (aw-window-list))))
  (defun goto-window-2 ()
    (interactive)
    (aw-switch-to-window (nth 1 (aw-window-list))))
  (defun goto-window-3 ()
    (interactive)
    (aw-switch-to-window (nth 2 (aw-window-list))))
  (defun goto-window-4 ()
    (interactive)
    (aw-switch-to-window (nth 3 (aw-window-list))))
  (defun goto-window-5 ()
    (interactive)
    (aw-switch-to-window (nth 4 (aw-window-list))))
  (defun goto-window-6 ()
    (interactive)
    (aw-switch-to-window (nth 5 (aw-window-list))))
  (setq aw-display-always nil)
  (add-hook 'window-configuration-change-hook 'aw-update)
  (defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (concat
                        (format-mode-line "%l:%c")
                        " - "
                        (window-parameter (selected-window) 'ace-window-path))))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position))))

;; --- all-the-icons
(use-package all-the-icons
  :config
  (require 'all-the-icons))

;; --- ansi-color
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; --- beginning-of-line-text
(global-set-key "\M-m" 'beginning-of-line-text)

;; --- caser
;; (use-package caser) ; broken with elpaca for some reason

;; --- compile
(global-set-key (kbd "C-c k") 'compile)
(setq compilation-scroll-output 'first-error)

;; --- copy-current-line-position-to-clipboard
(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as 'file:<path>::<line-number>'"
  (interactive)
  (let ((path-with-line-number
         (concat
          "file:" (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))
(defun copy-current-line-position-to-clipboard-org ()
  "Copy current line in file to clipboard as 'file:<path>::<line-number>'"
  (interactive)
  (let ((path-with-line-number
         (concat
          "[[file:" (buffer-file-name) "::" (number-to-string (line-number-at-pos))
          "][" (file-name-nondirectory (buffer-file-name)) " : " (which-function) " : "
          (string-trim (thing-at-point 'line t)) "]]")))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))
(global-set-key (kbd "C-c j j") 'copy-current-line-position-to-clipboard)
(global-set-key (kbd "C-c j k") 'copy-current-line-position-to-clipboard-org)

;; --- coverlay
(use-package coverlay)

;; --- crux
(use-package crux
  :bind (("C-c o" . crux-open-with)))

;; --- dart
(use-package dart-mode)

;; --- deadgrep
(use-package deadgrep
  :bind (("C-s-s" . #'deadgrep)))

;; --- default indentation
(setq-default indent-tabs-mode nil)

;; --- dead-keys
(setq ns-right-alternate-modifier 'none)

;; --- devdocs
(use-package devdocs
  :bind (("C-c d d" . devdocs-lookup))
  :init
  (setq devdocs-window-select t)
  :config
  (add-hook 'cmake-mode-hook
            (lambda () (setq-local devdocs-current-docs '("cmake~3.26"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-loadl devdocs-current-docs '("python~3.10")))))

;; --- dired
(setq dired-hide-details-mode t)

;; --- Dockerfiles
(use-package dockerfile-mode)

;; --- editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; --- eglot
(unload-feature 'eldoc t)
(setq custom-delayed-init-variables '())
(defvar global-eldoc-mode nil)

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode)) ;; This is usually enabled by default by Emacs
(use-package jsonrpc :ensure (:wait t) :defer t)
(use-package eglot
  :init
  :defer t
  :ensure (:wait t)
  :config
  ;; (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               '(TSX . ("typescript-language-server" "--stdio")))

  ;; Damn .projectile file
  (defun joaot/find-projectile-project ()
    (let ((probe (locate-dominating-file default-directory ".projectile")))
      (when probe `(projectile . ,probe))))

  (add-hook 'project-find-functions 'joaot/find-projectile-project 'append)

  :bind (("C-c a" . eglot-code-actions)
         ("C-c f" . eglot-format-buffer)
         ("C-M-." . eglot-find-typeDefinition)
         ("C-c e" . eglot-rename)))

;; --- Go
(use-package go-mode)

;; --- gptel
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  :bind (("C-c RET" . gptel-send)
         ("C-c g" . gptel)
         ("C-c h" . gptel-send)))

;; --- Emacs Everywhere
(use-package emacs-everywhere)

;; --- exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; --- expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; --- f
(use-package f)

;; --- FFAP
(global-set-key (kbd "C-c .") 'ffap)

;; --- flymake
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;; --- git auto-commit
(use-package git-auto-commit-mode)

;; --- golden-ratio-scroll-screen
(use-package golden-ratio-scroll-screen
  :config
  (require 'golden-ratio-scroll-screen)
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;; --- highlight
(setq highlight-nonselected-windows t)

;; --- ivy/swiper/counsel
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
  ;; (require 'nano-counsel)
  :demand t)
(use-package smex
  :ensure (:wait t))

(global-set-key (kbd "C-c u") 'counsel-unicode-char)

;; --- java
(add-hook 'java-mode-hook
          (lambda ()
            (set-fill-column 120)
            (display-fill-column-indicator-mode t)))

;; --- javascript
(add-hook 'typescript-mode
          (lambda ()
            (setq comment-line-break-function 'c-indent-new-comment-line)))

;; --- jq-mode
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

;; --- markdown-mode
(use-package markdown-mode)

;; --- multiple-cursors
(use-package multiple-cursors
  :bind
   (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))

(defun adviced:counsel-M-x-action (orig-fun &rest r)
  "Additional support for multiple cursors."
  (apply orig-fun r)
  (let ((cmd (intern (car r))))
    (when (and (boundp 'multiple-cursors-mode)
               multiple-cursors-mode
               cmd
               (not (memq cmd mc--default-cmds-to-run-once))
               (not (memq cmd mc/cmds-to-run-once))
               (or mc/always-run-for-all
                   (memq cmd mc--default-cmds-to-run-for-all)
                   (memq cmd mc/cmds-to-run-for-all)
                   (mc/prompt-for-inclusion-in-whitelist cmd)))
      (mc/execute-command-for-all-fake-cursors cmd))))

(advice-add #'counsel-M-x-action
            :around
            #'adviced:counsel-M-x-action)

;; --- open externally
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
       ((eq system-type 'linux) (executable-find "firefox"))
       ))
(defun w3mext-open-link-or-image-or-url ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (let (url)
    (if (string= major-mode "w3m-mode")
        (setq url (or (w3m-anchor) (w3m-image) w3m-current-url)))
    (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))
    ))
(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; --- org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
;(global-set-key (kbd "C-c g") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(org-babel-do-load-languages
'org-babel-load-languages
'((shell . t)
  (eshell . t)
  (python . t)))

; Disable indentation in code blocks
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(use-package ob-mermaid)

; Don't ask confirmation to evaluate some code blocks
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang (list "mermaid" "eshell" "python"))))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

; Live refresh inline images
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(use-package org-modern
  :init
  (setq org-modern-block-name nil)
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

;; ; Code to copy links out of org-mode
;; ; See https://emacs.stackexchange.com/a/3990
;; (defun org-extract-link-url (text)
;;   (string-match org-bracket-link-regexp text)
;;   (substring text (match-beginning 1) (match-end 1)))

;; (defun my-org-retrieve-url-from-point ()
;;   (interactive)
;;   (let* ((link-info (assoc :link (org-context)))
;;          (text (when link-info
;;                  ;; org-context seems to return nil if the current element
;;                  ;; starts at buffer-start or ends at buffer-end
;;                  (buffer-substring-no-properties (or (cadr link-info) (point-min))
;;                                                  (or (caddr link-info) (point-max)))))
;; 	 (url (org-extract-link-url text)))
;;     (if (not url)
;;         (error "Not in org link")
;;       (kill-new url))))

;; ;(require 'org-drawio)

(defun my-smarter-kill-ring-save ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))

(global-set-key (kbd "M-w") 'my-smarter-kill-ring-save)

(defun org-insert-backtick ()
  "Insert a backtick using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?`)
  (call-interactively #'org-self-insert-command))

(defun org-insert-tilde ()
  "Insert a tilde using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?~)
  (call-interactively #'org-self-insert-command))

(define-key org-mode-map (kbd "`") #'org-insert-tilde)
(define-key org-mode-map (kbd "~") #'org-insert-backtick)

;; --- org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c d t" . org-roam-dailies-goto-today)
         ("C-c d y" . org-roam-dailies-goto-yesterday)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;; ;; --- recentf
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 25
;;       recentf-max-saved-items 25)
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; (use-package restart-emacs)
;; (global-set-key (kbd "C-c r") 'restart-emacs)
;(use-package seq)

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))
(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))
(use-package transient
  :ensure (:wait t))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil)
  (define-key magit-mode-map (kbd "M-5") nil)
  (define-key magit-mode-map (kbd "M-6") nil)
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
                '(display-buffer-same-window)))))))


(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

;; --- prettier-js
(use-package prettier-js)

;; --- project.el - projectile
(use-package projectile
  :demand t
  :init
  (setq projectile-switch-project-action #'projectile-find-file)
  :config
  (projectile-mode +1)
  (projectile-load-known-projects)
  (projectile-commander-bindings)
  (defun project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))
  (add-hook 'project-find-functions #'project-override)
  :bind-keymap 
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map)))

(use-package counsel-projectile
  :config (counsel-projectile-mode t))

;; --- realgud
(use-package realgud
  :ensure (:wait t))
(use-package realgud-lldb)

;; --- rust
(use-package rust-mode)

;; ;; --- smartparens
;; (use-package smartparens)

;; ;; --- smithy
;; (use-package smithy-mode)

;; --- vterm
(use-package vterm)

(use-package vterm-toggle
  :bind (("C-;" . vterm-toggle)
         ("C-'" . vterm-toggle-cd))
  :config
  (setq vterm-toggle-project-root t)
  (setq vterm-toggle-scope 'project))

;; --- which-key
(use-package which-key
  :config
  (require 'which-key)
  (which-key-mode))

;; --- whitespace-mode
(global-set-key (kbd "C-c w") #'whitespace-mode)

;; --- winner-mode
(winner-mode)
(global-set-key (kbd "C-c ;") #'winner-undo)
(global-set-key (kbd "C-c '") #'winner-redo)

;; --- yaml-mode
(use-package yaml-mode)

;; (load-theme 'leuven)

;; --- nano
(elpaca (nano :host github
  			  :repo "rougier/nano-emacs")
  (require 'nano-layout)

  ;; Theming Command line options (this will cancel warning messages)
  (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
  (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
  (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

  ;; Theme
  (require 'nano-faces)
  (require 'nano-theme)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)

  (cond
   ((member "-default" command-line-args) t)
   ((member "-dark" command-line-args) (nano-theme-set-dark))
   (t (nano-theme-set-light)))
  (call-interactively 'nano-refresh-theme)

  ;; Nano default settings (optional)
  (require 'nano-defaults)

  ;; Nano session saving (optional)
  (require 'nano-session)

  ;; Nano header & mode lines (optional)
  (require 'nano-modeline)

  ;; --- What I like of nano-bindings
  ;; Close frame if not the last, kill emacs else
  (defun nano--delete-frame-or-kill-emacs ()
    "Delete frame or kill Emacs if there is only one frame."
    (interactive)
    (if (> (length (frame-list)) 1)
        (delete-frame)
      (save-buffers-kill-terminal)))
  (global-set-key (kbd "C-x C-c") 'nano--delete-frame-or-kill-emacs)

  ;; Open recent files 
  (global-set-key (kbd "C-c r") 'recentf-open-files)

  ;; Compact layout (need to be loaded after nano-modeline)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  
  ;; Nano counsel configuration (optional)
  ;; Needs "counsel" package to be installed (M-x: package-install)
  ;; (require 'nano-counsel)

  ;; Welcome message (optional)
  (let ((inhibit-message t))
    (message "Welcome to GNU Emacs / N Λ N O edition!")
    (message (format "Initialization time: %s" (emacs-init-time))))

  ;; Splash (optional)
  (unless (member "-no-splash" command-line-args)
    (require 'nano-splash))

  ;; Help (optional)
  (unless (member "-no-help" command-line-args)
    (require 'nano-help))
  (require 'nano-counsel))

;; --- Rest

(require 's)

(defun upper-camel-case (start end)
  "Replace the selected region with its contents converted to Upper Camel Case."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (s-upper-camel-case region-text))))
(global-set-key (kbd "C-c 1") 'upper-camel-case)
upart
