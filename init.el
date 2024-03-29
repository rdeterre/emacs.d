;; --- PRELUDE ---

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'org)

; Install MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(if (string-match-p "aarch64" system-configuration)
    (setq arch "aarch64")
  (setq arch "x86_64"))

(setq system
      (pcase system-type
        (darwin "macOS")
        (gnu/linux "Linux")
        (windows-nt "Windows")
        (_ "Unknown")))

(setq treesit-extra-load-path
      (list
       (concat
        (file-name-directory user-init-file)
        "tree-sitter-grammars/" system "/" arch)))

(add-to-list 'load-path "~/.emacs.d/lisp")
;(add-to-list 'load-path "~/Documents/axe")
;(require 'axe)


;; --- USER SETUP ---

;; ace-window
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; asdf
(require 'asdf)
(asdf-enable)

;; beginning-of-line-text
(global-set-key "\M-m" 'beginning-of-line-text)

;; compile
(global-set-key (kbd "C-c k") 'compile)
(setq compilation-scroll-output 'first-error)

;; coverlay
(use-package coverlay)

;; crux
(use-package crux
  :bind (("C-c o" . crux-open-with)))

;; dart
(use-package dart-mode)

;; deadgrep
(use-package deadgrep
  :bind (("C-s-s" . #'deadgrep)))

;; dead-keys
(setq ns-right-alternate-modifier 'none)

;; default indentation
(setq-default indent-tabs-mode nil)

;; dired
(setq dired-hide-details-mode t)

;; Dockerfiles
(use-package dockerfile-mode)

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package all-the-icons)
(require 'all-the-icons)

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; eglot
(use-package eglot
  :hook ((java-mode . eglot-ensure)
         (TSX . eglot-ensure)
         (rust-mode . eglot-ensure))
  :init
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(java-mode .
                           ("jdtls"
                            "--jvm-arg=-javaagent:/Volumes/brazil-pkg-cache/packages/Lombok/Lombok-1.18.x.27054.0/AL2_x86_64/DEV.STD.PTHREAD/build/lib/lombok-1.18.26.jar"
                            "-java.format.settings.url:https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                            "-java.format.settings.profile:GoogleStyle"
                              :initializationOptions (:extendedClientCapabilities (:classFileContentsSupport t )))))
  (add-to-list 'eglot-server-programs
               '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               '(TSX . ("typescript-language-server" "--stdio")))
  :bind (("C-c a" . eglot-code-actions)))
(global-set-key (kbd "C-c e") 'eglot-rename)
;; (setq eglot-workspace-configuration
;;       '(
;;          ("java.format.settings.url" . "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
;;          ("java.format.settings.profile" . "GoogleStyle")))

;; (setq eglot-workspace-configuration
;;             `(("java.format.settings.url" . "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")))

; NOTE: eglot passes tab-width along with formatting requests

;;; eclipse-jdt breaks the spec which in turn breaks code actions
;;; This behaviour can't be disabled and needs to be worked around
(cl-defmethod eglot-execute-command
  (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

;; The jdt server sometimes returns jdt:// scheme for jumping to definition
;; instead of returning a file. This is not part of LSP and eglot does not
;; handle it. The following code enables eglot to handle jdt files.
;; See https://github.com/yveszoundi/eglot-java/issues/6 for more info.
(defun jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir "/tmp/.eglot")
         (source-file
          (expand-file-name
           (file-name-concat
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (message "URI:%s" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (message "content:%s" content)
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . jdt-file-name-handler))

(defun jdthandler--wrap-legacy-eglot--path-to-uri (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a file path or potentially a URI.
If path is a jar URI, don't parse. If it is not a jar call ORIGINAL-FN."
  (let ((path (file-truename (car args))))
    (if (equal "jdt" (url-type (url-generic-parse-url path)))
        path
      (apply original-fn args))))

(defun jdthandler--wrap-legacy-eglot--uri-to-path (original-fn &rest args)
  "Hack until eglot is updated.
ARGS is a list with one element, a URI.
If URI is a jar URI, don't parse and let the `jdthandler--file-name-handler'
handle it. If it is not a jar call ORIGINAL-FN."
  (let ((uri (car args)))
    (if (and (stringp uri)
             (string= "jdt" (url-type (url-generic-parse-url uri))))
        uri
     (apply original-fn args))))
      

(defun jdthandler-patch-eglot ()
  "Patch old versions of Eglot to work with Jdthandler."
  (interactive) ;; TODO, remove when eglot is updated in melpa
  (unless (or (and (advice-member-p #'jdthandler--wrap-legacy-eglot--path-to-uri 'eglot--path-to-uri)
                   (advice-member-p #'jdthandler--wrap-legacy-eglot--uri-to-path 'eglot--uri-to-path))
              (<= 29 emacs-major-version))
    (advice-add 'eglot--path-to-uri :around #'jdthandler--wrap-legacy-eglot--path-to-uri)
    (advice-add 'eglot--uri-to-path :around #'jdthandler--wrap-legacy-eglot--uri-to-path)
    (message "[jdthandler] Eglot successfully patched.")))

; invoke
(jdthandler-patch-eglot)

;; Go
(use-package go-mode)

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

;; Emacs Everywhere
(use-package emacs-everywhere)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))


;; FFAP
(global-set-key (kbd "C-c .") 'ffap)

;; flymake
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;; fonts
(require 'init-fonts)

;; git gutter
;; See https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; graphviz
(use-package graphviz-dot-mode)

;; groovy
(use-package groovy-mode)

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

;; (use-package esh-mode
;;   :ensure nil
;;   :bind (:map eshell-mode-map
;; 	      ("C-r" . timmy/counsel-eshell-history)))

(global-set-key (kbd "C-c u") 'counsel-unicode-char)

(use-package counsel-projectile
  :config (counsel-projectile-mode t))

;; java
(add-hook 'java-mode-hook
          (lambda ()
            (set-fill-column 120)
            (display-fill-column-indicator-mode t)))

;; javascript
(add-hook 'typescript-mode
          (lambda ()
            (setq comment-line-break-function 'c-indent-new-comment-line)))

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

;; markdown-mode
(use-package markdown-mode)

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

;; org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c g") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package ein)

(org-babel-do-load-languages
'org-babel-load-languages
'((ein . t)
  (shell . t)
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

(defun org-quip-link-paste ()
  (interactive)
  (let* ((link (current-kill 0))
         (path (car (last (split-string link "/")))))
    (insert (format "[[%s][%s]]" link path))))
(define-key org-mode-map (kbd "C-c k") #'org-quip-link-paste)

(defun my-smarter-kill-ring-save ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))

(global-set-key (kbd "M-w") 'my-smarter-kill-ring-save)

;; org-roam
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
  (setq projectile-switch-project-action #'projectile-find-file)
  :config
  (projectile-mode +1)
  (projectile-load-known-projects)
  (projectile-commander-bindings)
  :bind-keymap 
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map)))


(defun projectile-shell-pop ()
  "Pop-up a shell buffer at the project root. Stolen from spacemacs "
  (interactive)
  (let ((default-directory (projectile-acquire-root)))
    (call-interactively 'shell-pop)))

(global-set-key (kbd "C-'") 'projectile-shell-pop)

(defun project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
      (cons 'vc override)
      nil)))

(add-hook 'project-find-functions #'project-override)

;; rust
(use-package rust-mode)

;; scala
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; shell-pop
(use-package shell-pop
  :init
  (setq shell-pop-term-shell "eshell")
  (setq shell-pop-shell-type '("eshell" "eshell" (lambda () (eshell))))
  (setq shell-pop-window-position "full")
  :bind
  (("C-t" . shell-pop)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-t") #'shell-pop)))

;; smartparens
(use-package smartparens)

;; smithy
(use-package smithy-mode)

;; tsx-mode
(use-package corfu)
(use-package origami)
(straight-use-package '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))
(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]s\\'" . tsx-mode))

; Fix for a crazy issue where typescript-ts-mode, automatically loaded
; by tsx-mode, adds itself to auto-mode-alist 🤯. This prevents
; tsx-mode from being loaded if we do nothing, so we need to remove
; it.
;
; See https://lists.gnu.org/archive/html/emacs-devel/2023-01/msg00263.html
(with-eval-after-load 'typescript-ts-mode
  (setq auto-mode-alist (delete '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist)))

;; typst
(use-package typst-mode)

;; which-key
(use-package which-key)
(require 'which-key)
(which-key-mode)

;; whitespace-mode
(global-set-key (kbd "C-c w") #'whitespace-mode)

;; winner-mode
(winner-mode)

;; yaml-mode
(use-package yaml-mode)
