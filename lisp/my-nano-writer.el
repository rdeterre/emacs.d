;; ---------------------------------------------------------------------  -*- lexical-binding: t; -*-
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers 
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
(require 'org)
(require 'org-indent)
(require 'org-element)
(require 'nano-base-colors)
(require 'nano-faces)

;; 
;;             prefix  
;;           |<------>|

;; border -> |<------>| * Headline level 1  # Unnumbered
;;           |<------>| 1 Headline level 1  # Numbered
;;
;;           |<----->| ** Headline level 2  # Unnumbered
;;           |<---->| 1.1 Headline level 2  # Numbered
;;
;;           |<---->| *** Headline level 3  # Unumbered
;;           |<-->| 1.1.1 Headline level 3  # Numbered
;; etc.
;;
;; This works if the number of sections at a given level is < 10.


(defun my-writer-mode--num-format (numbering)
  "Alternative numbering format for org-num.

First level: 1 | xxx
Second level: 1.1 — xxx
Third level: 1.1.1 - xxx
etc.
"""
  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "Roboto Condensed"
                          :height 140
                          :foreground ,nano-color-faded))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "Roboto Condensed"
                        :foreground ,nano-color-faded))))

;; Specific face for headline stars
(font-lock-add-keywords 'my-writer-mode
             '(("^*+ " 0 `(:family "Roboto Mono"
                           :height 140
                           :foreground ,nano-color-faded) prepend)
               ) 'append)

;;;###autoload
(define-derived-mode my-writer-mode org-mode "NΛNO writer"

  ;; Faces
  (face-remap-add-relative 'org-level-1
                           :overline nano-color-subtle
                           :family "Roboto" :height 140)
  (face-remap-add-relative 'org-level-2
                           :family "Roboto" :height 130)
  (face-remap-add-relative 'org-level-3
                           :family "Roboto" :height 120)
  (face-remap-add-relative 'org-document-info
                           :inherit 'nano-face-faded)
  (face-remap-add-relative 'org-document-title
                           :foreground nano-color-foreground
                           :family "Roboto Slab"
                           :height 150
                           :weight 'medium)
  ;; hide title / author ... keywords
  (setq-local org-hidden-keywords '(title author date startup))

  ;; Header line
  ;; (setq header-line-format nil)

  ;; Layout
  ;; (setq fill-column 72)
  (setq-default line-spacing 1)

  ;; Indentation
  (setq org-startup-folded nil)
  (setq org-level-color-stars-only nil)
  (setq org-hide-leading-stars nil)

  ;; Numbering
  (when (require 'org-num nil t)
    (setq org-num-skip-unnumbered t)
    (setq org-num-skip-footnotes t)
    (setq org-num-max-level 2)
    (setq org-num-face nil)
    (org-num-mode)
    (setq org-num-format-function 'my-writer-mode--num-format)))

(provide 'my-nano-writer)
