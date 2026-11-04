;;; my-folding.el --- Shared folding engines and indicators -*- lexical-binding: t; -*-

;;; Commentary:
;; Inspired by https://jointhefreeworld.org/blog/articles/emacs/emacs-folding-kirigami/
;; Prefer Tree-sitter, then Hideshow for code, and Outline for text headings.
;; Org indicators cover visible headings only, regardless of document size.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'hideshow)
(require 'outline)

(defvar treesit-fold-mode)
(defvar kirigami-fold-list)
(defvar my-org-fold-indicators-mode)
(defvar org-heading-regexp)

(declare-function nxml-scan-element-forward "nxml-rap" (from))
(declare-function treesit-fold-indicators-mode "treesit-fold-indicators" (&optional arg))
(declare-function org-invisible-p "org" (&optional pos))
(declare-function org-at-heading-p "org" (&optional invisible-ok))
(declare-function org-fold-folded-p "org-fold" (&optional pos spec))
(declare-function treesit-fold--foldable-node-at-pos "treesit-fold" (&optional pos))
(declare-function kirigami--normalize-point "kirigami" ())
(declare-function kirigami--treesit-fold-normalize-point "kirigami" ())
(declare-function kirigami-toggle-fold "kirigami" ())
(declare-function kirigami-open-folds "kirigami" ())
(declare-function kirigami-close-folds "kirigami" ())

(defface my-folding-indicator
  '((t :inherit shadow :weight normal))
  "Subtle fringe markers shared by folding engines."
  :group 'convenience)

(defun my-folding-refresh-faces (&rest _)
  "Blend folding indicators halfway from shadow toward the background."
  (let* ((fg (color-name-to-rgb (face-foreground 'shadow nil t)))
         (bg (color-name-to-rgb (face-background 'default nil t)))
         (color (when (and fg bg)
                  (apply #'color-rgb-to-hex
                         (cl-mapcar (lambda (a b) (/ (+ a b) 2)) fg bg)))))
    (dolist (face '(my-folding-indicator treesit-fold-fringe-face
                   hs-indicator-hide hs-indicator-show))
      (when (facep face)
        (set-face-attribute face nil :inherit 'shadow :weight 'normal
                            :foreground (or color 'unspecified)
                            :background 'unspecified :box nil)))))

(setq hs-show-indicators t
      hs-indicator-type 'fringe)
(add-hook 'enable-theme-functions #'my-folding-refresh-faces)
(add-hook 'disable-theme-functions #'my-folding-refresh-faces)
(add-hook 'after-make-frame-functions #'my-folding-refresh-faces)
(my-folding-refresh-faces)

(defun my-folding-xml-forward (_arg)
  "Move over the XML element starting at point."
  (goto-char (nxml-scan-element-forward (point))))

(defun my-folding-configure-hideshow ()
  "Set buffer-local block rules before enabling Hideshow."
  (cond
   ((derived-mode-p 'scheme-mode)
    (setq-local hs-block-start-regexp "^("
                hs-forward-sexp-function #'forward-sexp))
   ((derived-mode-p 'nxml-mode)
    ;; Exclude declarations, comments and self-closing elements.
    (setq-local hs-block-start-regexp "<[[:alpha:]_][^>]*[^/>]>\\|<[[:alpha:]_]>"
                hs-block-end-regexp "</[^>]+>"
                hs-forward-sexp-function #'my-folding-xml-forward))
   ((derived-mode-p 'yaml-mode)
    ;; Emacs 31 provides the indentation engine used for this fallback.
    (hs-indentation-mode 1))))

(defun my-folding-outline-buttons (enabled)
  "Use Outline buttons only when ENABLED, without duplicating other markers."
  (unless (eq outline-minor-mode-use-buttons enabled)
    (let ((active (bound-and-true-p outline-minor-mode)))
      (when active (outline-minor-mode -1))
      (setq-local outline-minor-mode-use-buttons enabled)
      (when active (outline-minor-mode 1)))))

(defun my-folding-setup ()
  "Choose one folding engine and indicator set for the current buffer."
  (unless (minibufferp)
    (cond
     ((derived-mode-p 'org-mode)
      (my-org-fold-indicators-mode 1))
     ((bound-and-true-p treesit-fold-mode)
      (when (bound-and-true-p hs-minor-mode) (hs-minor-mode -1))
      (my-folding-outline-buttons nil)
      (unless (bound-and-true-p treesit-fold-indicators-mode)
        (treesit-fold-indicators-mode 1)))
     ((derived-mode-p 'prog-mode 'conf-mode 'nxml-mode 'yaml-mode)
      (my-folding-outline-buttons nil)
      (unless (bound-and-true-p hs-minor-mode)
        (my-folding-configure-hideshow)
        (hs-minor-mode 1)))
     ((or (derived-mode-p 'markdown-mode 'markdown-ts-mode 'diff-mode)
          (bound-and-true-p outline-minor-mode))
      (my-folding-outline-buttons t)
      (unless (bound-and-true-p outline-minor-mode)
        (outline-minor-mode 1))))))

(defun my-folding-treesit-changed ()
  "Keep fallback engines and indicators in sync with Tree-sitter folding."
  (when (and (not treesit-fold-mode)
             (bound-and-true-p treesit-fold-indicators-mode))
    ;; The indicator mode tries to enable its engine even when disabling.
    (let ((treesit-fold-mode t))
      (treesit-fold-indicators-mode -1)))
  (my-folding-setup))

(defun my-folding-refresh-buffers ()
  "Configure existing buffers after the asynchronous package load."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer (my-folding-setup))))

(defun my-folding-hs-open-recursively ()
  "Open the Hideshow block at point and any nested hidden blocks."
  (save-excursion
    (when (hs-get-near-block)
      (pcase-let ((`(,beg ,end) (hs-block-positions)))
        (when (and beg end) (hs-discard-overlays beg end))))))

(defun my-folding-configure-kirigami ()
  "Give active code engines precedence without discarding other handlers."
  (dolist (mode '(hs-minor-mode treesit-fold-mode))
    (let ((entries (cl-remove-if-not
                    (lambda (entry) (memq mode (car entry))) kirigami-fold-list)))
      (setq kirigami-fold-list
            (append entries (cl-remove-if
                             (lambda (entry) (memq mode (car entry)))
                             kirigami-fold-list)))
      (when (eq mode 'hs-minor-mode)
        (dolist (entry entries)
          (setcdr entry (plist-put (cdr entry) :open-rec
                                   #'my-folding-hs-open-recursively))))))
  (add-hook 'kirigami-post-action-functions #'my-folding-track-global-action))

(defvar-local my-folding--all-closed nil
  "Non-nil when the last buffer-wide fold action closed everything.")

(defun my-folding-track-global-action (action)
  "Remember whether buffer-wide ACTION closed or opened all folds."
  (pcase action
    (:close-all (setq my-folding--all-closed t))
    (:open-all (setq my-folding--all-closed nil))))

(defun my-folding-foldable-at-point-p ()
  "Return non-nil when the active engine has a fold at point."
  (save-excursion
    (cond
     ((bound-and-true-p treesit-fold-mode)
      (kirigami--treesit-fold-normalize-point)
      (treesit-fold--foldable-node-at-pos))
     ((bound-and-true-p hs-minor-mode)
      (kirigami--normalize-point)
      (or (hs-already-hidden-p) (hs-get-near-block t)))
     ((or (derived-mode-p 'outline-mode)
          (bound-and-true-p outline-minor-mode))
      (ignore-errors (outline-back-to-heading t) t))
     (t t))))

(defun my-folding-toggle ()
  "Toggle the fold at point, or every fold when there is none at point."
  (interactive)
  (cond
   ((my-folding-foldable-at-point-p) (kirigami-toggle-fold))
   (my-folding--all-closed (kirigami-open-folds))
   (t (kirigami-close-folds))))

;; The repeating guide fills any line height, including extra line spacing.
(define-fringe-bitmap 'my-folding-guide [24] nil nil '(top t))

(defun my-folding-bitmap (kind window)
  "Return a bitmap for KIND sized to a line in WINDOW."
  (if (eq kind 'guide)
      'my-folding-guide
    (with-current-buffer (window-buffer window)
      (let* ((font-height (window-font-height window))
             (spacing (cond ((integerp line-spacing) line-spacing)
                            ((floatp line-spacing) (round (* line-spacing font-height)))
                            (t 0)))
             (height (max 9 (+ font-height spacing)))
             (name (intern (format "my-folding-%s-%d" kind height))))
        (unless (get name 'my-folding-defined)
          (let* ((rows (make-vector height 0))
                 (middle (/ height 2))
                 (top (/ (- height 7) 2)))
            (if (memq kind '(end-left end-right))
                (dotimes (i (+ middle 2))
                  (aset rows i (if (< i middle) 24
                                 (if (eq kind 'end-left) 31 248))))
              (let ((box (if (eq kind 'closed)
                             [127 65 73 93 73 65 127]
                           [127 65 65 93 65 65 127])))
                (dotimes (i 7) (aset rows (+ top i) (aref box i)))
                (when (eq kind 'open)
                  (cl-loop for i from (+ top 7) below height do (aset rows i 24)))))
            (define-fringe-bitmap name rows nil nil 'top)
            (put name 'my-folding-defined t)))
        name))))

(defun my-folding-indicator-string (string)
  "Replace the bitmap in Tree-sitter indicator STRING with a continuous guide."
  (when string
    (let* ((display (get-text-property 0 'display string))
           (kind (pcase (nth 1 display)
                   ('treesit-fold-indicators-fr-plus 'closed)
                   ('treesit-fold-indicators-fr-minus-tail 'open)
                   ('treesit-fold-indicators-fr-center 'guide)
                   ('treesit-fold-indicators-fr-end-left 'end-left)
                   ('treesit-fold-indicators-fr-end-right 'end-right))))
      (when kind
        (let ((window (or (get-buffer-window) (selected-window))))
          (put-text-property 0 (length string) 'display
                             (list (car display) (my-folding-bitmap kind window)
                                   (nth 2 display)) string)))
      string)))

(with-eval-after-load 'treesit-fold-indicators
  (advice-add 'treesit-fold-indicators--get-string :filter-return
              #'my-folding-indicator-string)
  (my-folding-refresh-faces))
(add-hook 'treesit-fold-mode-hook #'my-folding-treesit-changed)
(add-hook 'after-change-major-mode-hook #'my-folding-setup 90)

;; Org owns its visibility.  Keep only viewport indicators, reuse overlays,
;; and query Org each time so TAB, search and external commands stay in sync.
(defvar-local my-org-fold-indicators--overlays nil)

(defun my-org-fold-indicators-clear ()
  "Remove this buffer's Org indicators."
  (mapc #'delete-overlay my-org-fold-indicators--overlays)
  (setq my-org-fold-indicators--overlays nil))

(defun my-org-fold-indicators-refresh (&rest _)
  "Refresh visible heading markers in every window displaying this buffer."
  (when my-org-fold-indicators-mode
    (let ((old my-org-fold-indicators--overlays) fresh)
      (save-excursion
        (save-match-data
          (dolist (window (get-buffer-window-list nil nil t))
            (let ((end (save-excursion
                         (goto-char (window-start window))
                         (vertical-motion (window-body-height window) window)
                         (point))))
              (goto-char (window-start window))
              (beginning-of-line)
              (while (re-search-forward org-heading-regexp end t)
                (let ((beg (line-beginning-position)))
                  (if (org-invisible-p beg)
                      ;; A visible range can span a huge collapsed subtree.
                      ;; Jump across it instead of visiting hidden headings.
                      (goto-char (next-single-char-property-change beg 'invisible nil end))
                    (let* ((ov (cl-find-if
                                (lambda (ov)
                                  (and (eq (overlay-get ov 'window) window)
                                       (eq (overlay-start ov) beg))) old))
                           (closed (org-fold-folded-p (line-end-position) 'outline))
                           (bitmap (if closed 'right-triangle 'down-triangle)))
                      (if ov (setq old (delq ov old))
                        (setq ov (make-overlay beg (1+ beg) nil t nil))
                        (overlay-put ov 'window window)
                        (overlay-put ov 'evaporate t))
                      (overlay-put ov 'before-string
                                   (propertize " " 'display
                                               `(left-fringe ,bitmap my-folding-indicator)))
                      (push ov fresh)))))))))
      (mapc #'delete-overlay old)
      (setq my-org-fold-indicators--overlays fresh))))

(defun my-org-fold-indicators-scroll (window _start)
  "Refresh Org indicators when WINDOW scrolls."
  (with-current-buffer (window-buffer window)
    (when (bound-and-true-p my-org-fold-indicators-mode)
      (my-org-fold-indicators-refresh))))

(defun my-org-fold-indicators-click (event)
  "Toggle the Org heading beside fringe mouse EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (window (posn-window pos))
         (row (cdr (posn-col-row pos))))
    (when (and (windowp window) (integerp row))
      (select-window window)
      (move-to-window-line row)
      (beginning-of-line)
      (when (org-at-heading-p) (org-cycle))
      (my-org-fold-indicators-refresh))))

(define-minor-mode my-org-fold-indicators-mode
  "Show clickable, viewport-only Org folding triangles."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [left-fringe mouse-1] #'my-org-fold-indicators-click)
            map)
  (if my-org-fold-indicators-mode
      (progn
        (add-hook 'post-command-hook #'my-org-fold-indicators-refresh nil t)
        (add-hook 'window-scroll-functions #'my-org-fold-indicators-scroll nil t)
        (add-hook 'change-major-mode-hook #'my-org-fold-indicators-clear nil t)
        (my-org-fold-indicators-refresh))
    (remove-hook 'post-command-hook #'my-org-fold-indicators-refresh t)
    (remove-hook 'window-scroll-functions #'my-org-fold-indicators-scroll t)
    (remove-hook 'change-major-mode-hook #'my-org-fold-indicators-clear t)
    (my-org-fold-indicators-clear)))

(my-folding-refresh-buffers)

(provide 'my-folding)
;;; my-folding.el ends here
