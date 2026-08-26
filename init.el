;; --- elpaca prelude  -*- lexical-binding: t; -*-
;; Bootstraps elpaca and sets up the use-package integration

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;; --- Standard Emacs prelude
;; Changes values of some default Emacs variables
(add-to-list 'load-path "~/.emacs.d/lisp")
(if (string-equal system-type "darwin")
    (progn
      (setq mac-option-key-is-meta nil
            mac-command-key-is-meta t
            mac-command-modifier 'meta
            mac-option-modifier 'super
            frame-resize-pixelwise t
            ns-use-native-fullscreen nil
            ns-function-modifier 'hyper)
      (global-set-key (kbd "H-C-f") #'toggle-frame-maximized)))


(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-revert-verbose nil
      column-number-mode t
      custom-file "~/.emacs.d/custom.el"
      gc-cons-threshold 80000000
      highlight-nonselected-windows t
      initial-major-mode 'org-mode
      vc-follow-symlinks t  ; suppress "follow symlink into git repo?" prompt on M-.
      visible-bell t
      comint-scroll-to-bottom-on-output t
      native-comp-async-report-warnings-errors 'silent) ; suppress warnings about "not-known to be defined" symbols
(global-auto-revert-mode 1)
(setq-default fill-column 120
              indent-tabs-mode nil
              tab-width 2
              eglot-inlay-hints-mode nil)
(global-unset-key (kbd "M-<return>"))

(defun ansi-colorize ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

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
  (add-hook 'window-configuration-change-hook 'aw-update))

(use-package shell-maker
  ;; :ensure (:host github :repo "rdeterre/shell-maker" :ref "scheduled-message")
  :ensure (:host github :repo "xenodium/shell-maker" :ref "main")
  )

;; --- agent-shell
(use-package agent-shell
  ;; :ensure (:host github :repo "rdeterre/agent-shell" :ref "scheduled-message")
  :ensure (:host github :repo "xenodium/agent-shell" :ref "main")
  :after (shell-maker)
  :bind
  (("C-c c" . agent-shell)
   :map agent-shell-mode-map
   ("<tab>" . agent-shell-ui-toggle-fragment-at-point))
  :config
  (setq agent-shell-header-style 'text
        agent-shell-show-welcome-message nil
        agent-shell-context-sources '(files region error))
  ;; (defun my/agent-shell-display-buffer (buffer _alist)
;;     "Display agent-shell BUFFER stacked vertically on the right side of the frame.
;; If other agent-shell windows exist, split below the bottommost one.
;; Otherwise, replace the rightmost window with the agent-shell buffer."
;;     (let* ((shell-wins (seq-filter
;;                         (lambda (w)
;;                           (and (not (eq (window-buffer w) buffer))
;;                                (with-current-buffer (window-buffer w)
;;                                  (derived-mode-p 'agent-shell-mode))))
;;                         (window-list)))
;;            (bottom-win (when shell-wins
;;                          (car (seq-sort-by #'window-top-line #'> shell-wins)))))
;;       (if bottom-win
;;           (let ((new-win (split-window bottom-win nil 'below)))
;;             (set-window-buffer new-win buffer)
;;             new-win)
;;         (let* ((root (frame-root-window))
;;                (right-win (or (window-in-direction 'right root t)
;;                               (let ((wins (window-list nil nil root)))
;;                                 (car (seq-sort-by #'window-left-column #'> wins)))))
;;                (target (if (and right-win (not (eq right-win root)))
;;                            right-win
;;                          (split-window root nil 'right))))
;;           (set-window-buffer target buffer)
;;           target))))
;;   (setq agent-shell-display-action '(my/agent-shell-display-buffer))
  (add-hook 'window-selection-change-functions
            (lambda (_frame)
              (when (derived-mode-p 'agent-shell-mode)
                (agent-shell-jump-to-latest-permission-button-row)))))

;; --- claude-code region reference
(defun claude-code-copy-region (start end)
  "Copy a Claude Code-friendly reference for the active region to the kill ring.
Format: relative-or-absolute-path:start_line-end_line followed by the
numbered code content, matching what agent-shell sends to a shell."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((file (buffer-file-name))
         (root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (rel-file (if (and file root (file-in-directory-p file root))
                       (file-relative-name file root)
                     file))
         (line-start (line-number-at-pos start))
         (line-end (line-number-at-pos (if (and (> end start)
                                                (= (char-before end) ?\n))
                                           (1- end)
                                         end)))
         (header (if rel-file
                     (format "%s:%d-%d" rel-file line-start line-end)
                   (format "lines %d-%d" line-start line-end)))
         (lines (save-excursion
                  (goto-char start)
                  (beginning-of-line)
                  (let ((result '())
                        (current line-start))
                    (while (<= current line-end)
                      (push (format "%d: %s" current
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                            result)
                      (forward-line 1)
                      (setq current (1+ current)))
                    (nreverse result))))
         (text (concat header "\n" (string-join lines "\n"))))
    (kill-new text)
    (message "Copied: %s" header)))

(global-set-key (kbd "C-c C-y") 'claude-code-copy-region)

;; --- all-the-icons
(use-package all-the-icons
  :config
  (require 'all-the-icons))

;; --- vertico
(use-package vertico
  :init
  (vertico-mode))

;; --- orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --- marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; --- embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("M-o" . embark-act)
   :map embark-file-map
   ("v" . magit-status))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ; Make magit-status apply to the vertico-selected file/project, not the current one
  (add-to-list 'embark-around-action-hooks '(magit-status embark--cd)))

;; --- consult
(defun my/consult-ripgrep-project (&optional choose-directory)
  "Incrementally search the current project with ripgrep.
With a prefix argument, prompt for a directory to search instead."
  (interactive "P")
  (require 'project)
  (require 'consult)
  (let ((directory
         (if choose-directory
             (read-directory-name "Ripgrep directory: " default-directory nil t)
           (if-let ((project (project-current nil)))
               (project-root project)
             (user-error "No project found for %s" default-directory)))))
    (consult-ripgrep directory)))

(use-package consult
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("C-s"     . consult-line)
   ("M-s l"   . consult-line)
   ("M-s r"   . consult-ripgrep)
   ("C-c s"   . my/consult-ripgrep-project)
   ("C-x C-r" . consult-recent-file)
   :map isearch-mode-map
   ("M-l"     . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (recentf-mode 1)
  :config
  ;; The word at point is the first future-history item (M-n), not the line.
  (consult-customize consult-line :default nil))

;; --- embark-consult
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; --- vertico-posframe (show minibuffer centered on screen)
(use-package posframe)

(use-package vertico-posframe
  :after vertico
  :demand t
  :bind (:map vertico-map
         ("M-l" . vertico-posframe-toggle))
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center
        vertico-posframe-width 100
        vertico-posframe-border-width 2)
  (defun vertico-posframe-toggle ()
    (interactive)
    (vertico-posframe-mode (if vertico-posframe-mode -1 1))))

;; --- ansi-color
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; --- beginning-of-line-text
(global-set-key "\M-m" 'beginning-of-line-text)

;; --- caser
;; TODO: check
;; (use-package caser) ; broken with elpaca for some reason

;; --- casual
(use-package casual
  :after transient
  :config
  (casual-init))

;; --- clang-format
(use-package clang-format)

;; --- cmake
(use-package cmake-mode
  :mode "\\.cmake$")

;; --- code-cells
(use-package code-cells)

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

(use-package csv-mode
  :mode "\\.csv$")

;; --- dart
(use-package dart-mode
  :mode "\\.dart$")

;; --- deadgrep
(use-package deadgrep
  :bind (("C-s-s" . #'deadgrep)))

;; --- default indentation
(setq-default indent-tabs-mode nil)

;; --- dead-keys
(setq ns-right-alternate-modifier 'none)


(setq use-nano nil)

;; --- devdocs
(use-package devdocs
  :bind (("C-c d d" . devdocs-lookup))
  :init
  (setq devdocs-window-select t)
  :config
  (add-hook 'cmake-mode-hook
            (lambda () (setq-local devdocs-current-docs '("cmake~3.26"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-loadl devdocs-current-docs '("python~3.10"))))
  (defun devdocs-nano-modeline ()
    (setq-local header-line-format
                (let ((buffer-name (format-mode-line "%b"))
                      (mode-name (nano-mode-name))
                      (branch (eval (cadr devdocs-header-line)))
                      (position (format-mode-line "%l:%c")))
                  (nano-modeline-compose (nano-modeline-status)
                                         buffer-name
                                         (concat "(" mode-name
                                                 (if branch (concat ", "
                                                                    (propertize branch 'face 'italic)))
                                                 ")" )
                                         position))))
  (if use-nano
    (add-hook 'devdocs-mode-hook #'devdocs-nano-modeline)))

;; --- disaster
(use-package disaster)
    
;; --- dired
(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . auto-revert-mode))
  :config
  (setq dired-recursive-copies 'always
        delete-by-moving-to-trash t
        ;; Adding human readable units
        ;; -A List all entries except for "." and ".."
        ;; -l List in long format
        ;; -h Use human readable units
        dired-listing-switches "-Alh"
        dired-dwim-target t
        dired-auto-revert-buffer t
        dired-omit-files "^\\..*$\\|^\\.||.$"
        dired-omit-mode t)
  (put 'dired-find-alternate-file 'disabled nil))
(use-package dired-collapse
  :after dired-x
  :config
  (require 'dired-x)
  :ensure t)
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config
                                        ; Make all faces in subtree use default background
  (dolist (face (custom-group-members 'dired-subtree-faces nil))
    (when (and (consp face) (eq (cadr face) 'custom-face))
      (set-face-attribute (car face) nil :background 'unspecified))))


;; --- Dockerfiles
(use-package dockerfile-mode
  :mode "Dockerfile")

;; --- ediff
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

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
  :ensure nil
  :config
  ;; (advice-add 'jsonrpc--log-event :override #'ignore)
  ;; (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs
               '(TSX . ("typescript-language-server" "--stdio")))
  (if (string-equal system-type "darwin")
      (setq eglot-code-action-indicator "*"))

  ;; Damn .projectile file
  (defun joaot/find-projectile-project ()
    (let ((probe (locate-dominating-file default-directory ".projectile")))
      (when probe `(projectile . ,probe))))

  (add-hook 'project-find-functions 'joaot/find-projectile-project 'append)

  ;; (defun romain/maybe-start-eglot ()
  ;;   "Start eglot if not already running and a server is known for this mode."
  ;;   (when (and (not (eglot-current-server))
  ;;              (assoc major-mode eglot-server-programs))
  ;;     (eglot-ensure)))

  ;; (dolist (cmd '(xref-find-definitions
  ;;                xref-find-definitions-other-window
  ;;                xref-find-references
  ;;                xref-find-apropos))
  ;;   (advice-add cmd :before #'romain/maybe-start-eglot))

  :bind (("C-c a" . eglot-code-actions)
         ("C-c f f" . eglot-format-buffer)
         ("C-M-." . eglot-find-typeDefinition)
         ("C-c r" . eglot-rename)))

(defun romain/eglot-workspace-symbols ()
  "Search workspace symbols via Eglot using completing-read."
  (interactive)
  (unless (eglot-current-server)
    (user-error "No Eglot server running"))
  (let* ((sym-kinds ["File" "Module" "Namespace" "Package" "Class" "Method"
                     "Property" "Field" "Constructor" "Enum" "Interface"
                     "Function" "Variable" "Constant" "String" "Number"
                     "Boolean" "Array" "Object" "Key" "Null" "EnumMember"
                     "Struct" "Event" "Operator" "TypeParameter"])
         (server (eglot-current-server))
         (query (read-string "Workspace symbol query: "))
         (result (jsonrpc-request server :workspace/symbol `(:query ,query)))
         (candidates
          (mapcar
           (lambda (sym)
             (let* ((name (plist-get sym :name))
                    (kind (plist-get sym :kind))
                    (container (plist-get sym :containerName))
                    (kind-str (if (and kind (> kind 0) (<= kind (length sym-kinds)))
                                  (aref sym-kinds (1- kind)) "?"))
                    (display (if (and container (not (string-empty-p container)))
                                 (format "%-40s %-15s %s" name kind-str container)
                               (format "%-40s %s" name kind-str))))
               (propertize display 'eglot-location (plist-get sym :location))))
           (if (vectorp result) (append result nil) result)))
         (choice (completing-read "Symbol: " candidates nil t)))
    (when-let* ((loc (get-text-property 0 'eglot-location choice))
                (uri (plist-get loc :uri))
                (start (plist-get (plist-get loc :range) :start))
                (file (eglot--uri-to-path uri)))
      (find-file file)
      (goto-char (point-min))
      (forward-line (plist-get start :line))
      (forward-char (plist-get start :character)))))

(global-set-key (kbd "C-c C-s") #'romain/eglot-workspace-symbols)
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c C-s") nil)
  (define-key c-mode-map (kbd "C-c C-s") nil))

;; --- git-link
(use-package git-link
  :bind (("C-c f l" . git-link)
         ("C-c f c" . git-link-commit)
         ("C-c f h" . git-link-homepage)))

;; --- Go
(use-package go-mode
  :mode "\\.go")

;; --- gptel
(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode
        gptel-mode 'mistral-small
        gptel-backend (gptel-make-openai "MistralLeChat"
                        :host "api.mistral.ai"
                        :endpoint "/v1/chat/completions"
                        :protocol "https"
                        :key "0NKvFWiOkUhpVvelYTRcsfhYQrUCduDd"
                        :models '("mistral-small")))
  :bind (("C-c RET" . gptel-send)
         ("C-c g" . gptel)
         ("C-c h" . gptel-send)))

;; --- grip - Github Readme Instant Preview
(use-package grip-mode
  :config (setq grip-command 'auto) ;; auto, grip, go-grip or mdopen
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

;; --- Emacs Everywhere
(use-package emacs-everywhere)

;; --- exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; --- expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

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

(global-set-key (kbd "C-c u") 'insert-char)

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
(use-package markdown-mode
  :hook (markdown-mode . valign-mode)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; Prevent M-q from merging separate list items
              (setq-local paragraph-start
                          (concat paragraph-start "\\|\\s-*[-+*]\\s-")))))

(use-package valign)

(defun md-to-org (start end)
  "Convert the markdown region between START and END to org-mode in place."
  (interactive "r")
  (let ((result (shell-command-to-string
                 (format "echo %s | pandoc -f markdown -t org"
                         (shell-quote-argument (buffer-substring-no-properties start end))))))
    (delete-region start end)
    (insert result)))

;; --- multiple-cursors
(use-package multiple-cursors
  :bind
   (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)))


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

(defun git-sync ()
  (interactive)
  (magit-checkout "master")
  (magit-status)
  (magit-fetch-all ())
  (magit-pull-from-upstream nil)
  (revert-buffer-quick))
(global-set-key (kbd "C-c h") #'git-sync)

(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

(defun forward-to-word-start (&optional n)
  "Move to the beginning of the next word."
  (interactive "^p")
  (dotimes (_ (or n 1))
    (skip-syntax-forward "w")
    (skip-syntax-forward "^w")))
(global-set-key (kbd "M-F") #'forward-to-word-start)

(defun backward-to-word-end (&optional n)
  "Move to the end of the previous word."
  (interactive "^p")
  (dotimes (_ (or n 1))
    (skip-syntax-backward "w")
    (skip-syntax-backward "^w")))
(global-set-key (kbd "M-B") #'backward-to-word-end)

;; --- ghostel
(use-package ghostel
  :ensure (:host github :repo "dakra/ghostel")
  :custom
  (ghostel-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
     "M-o" "M-1" "M-2" "M-3" "M-4" "M-5" "M-6")))

;; --- pdf-tools
(use-package pdf-tools)

;; --- prettier-js
(use-package prettier-js)

;; --- project.el - projectile
(use-package projectile
  :ensure (:host github :repo "rdeterre/projectile" :ref "ghostel")
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


(global-set-key (kbd "C-'") 'projectile-run-vterm)
(global-set-key (kbd "C-c '") 'projectile-run-vterm)
(global-set-key (kbd "C-c C-k") 'kill-compilation)

;; --- python
(use-package python-black)

;; --- realgud
;; (use-package realgud
;;   :ensure (:wait t))
;; (use-package realgud-lldb)

;; --- ruff
(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

;; --- rust
(use-package rust-mode
  :mode "\\.rs")

;; ;; --- smartparens
;; (use-package smartparens)

;; --- Tree-sitter
(defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
(add-to-list 'auto-mode-alist '("\\.ts$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

;; --- treesit-fold
(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :bind (("C-c f ." . treesit-fold-toggle)
         ("C-c f o" . treesit-fold-open-all)
         ("C-c f l" . treesit-fold-close-all)
         ("C-c . " . treesit-fold-toggle))
  :config
  (setq treesit-fold-line-count-show t)
  (global-treesit-fold-mode 1))

;; --- vterm
(use-package vterm
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key))
  :init
  (setq vterm-max-scrollback 50000)
  :config
  (define-key vterm-mode-map (kbd "M-1") nil)
  (define-key vterm-mode-map (kbd "M-2") nil)
  (define-key vterm-mode-map (kbd "M-3") nil)
  (define-key vterm-mode-map (kbd "M-4") nil)
  (define-key vterm-mode-map (kbd "M-5") nil)
  (define-key vterm-mode-map (kbd "M-6") nil)
  (defun disable-vterm-copy-mode ()
    (interactive)
    (vterm-copy-mode -1))
  (define-key vterm-copy-mode-map (kbd "<return>") #'disable-vterm-copy-mode)

  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (add-hook 'vterm-mode-hook 'set-no-process-query-on-exit))

(defun named-vterm (term-name)
  (interactive "sTerminal purpose: ")
  (vterm (concat "vterm-" term-name)))

(defun named-ghostel (term-name)
  (interactive "sTerminal purpose: ")
  (let ((ghostel-buffer-name (concat "*ghostel-" term-name "*")))
    (ghostel)))
(global-set-key (kbd "C-c C-n") #'named-ghostel)

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
(if use-nano
    (elpaca (nano :host github
  			          :repo "rougier/nano-emacs")
      (setq nano-font-family-monospaced "Fira Mono")
      ;; (setq nano-font-family-monospaced "SF Mono")
      (setq nano-font-size 12)
      ;; (call-interactively 'nano-refresh-theme)
      (require 'nano-layout)

      ;; Theming Command line options (this will cancel warning messages)
      (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
      (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
      (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
      (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
      (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
      (add-to-list 'command-switch-alist '("-compact" . (lambda (args))))
      (setq nano-font-family-proportional "San Francisco")

      ;; Theme
      (require 'nano-faces)
      (require 'nano-theme)
      (require 'nano-theme-dark)
      (require 'nano-theme-light)

      ;; Nano session saving (optional)
      (require 'nano-session)

      ;; Nano header & mode lines (optional)
      (require 'nano-modeline)

      (defun nano-modeline-term-mode ()
        (nano-modeline-compose " >_ "
                               (buffer-name)
                               (concat "(" shell-file-name ")")
                               (shorten-directory default-directory 32)))

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

      (require 'my-nano-writer)
      (add-to-list 'auto-mode-alist '("\\.org$" . my-writer-mode))

      (cond
       ((member "-default" command-line-args) t)
       ((member "-dark" command-line-args) (nano-theme-set-dark))
       (t (nano-theme-set-light)))
      (call-interactively 'nano-refresh-theme)

      (set-face-attribute 'nano-face-strong nil
                          :foreground nano-color-strong
                          :weight 'bold)
      (set-face 'font-lock-variable-name-face 'default)
      (set-face 'org-list-dt 'nano-face-strong)
      (global-set-key (kbd "M-p") 'flymake-goto-prev-error)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))
(unless use-nano
  (progn
    (load-theme 'leuven)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))


(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; --- Rest

(require 's)

(defun upper-camel-case (start end)
  "Replace the selected region with its contents converted to Upper Camel Case."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (s-upper-camel-case region-text))))
(global-set-key (kbd "C-c 1") 'upper-camel-case)
