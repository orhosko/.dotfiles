(use-package visual-fill-column
  :ensure t
  :config
  ;;; Centering Org Documents --------------------------------
  ;; Configure fill width
  (customize-set-variable 'visual-fill-column-width 120)
  (customize-set-variable 'visual-fill-column-center-text t))

;;; Theme and Fonts ----------------------------------------

       ;; Set reusable font name variables
       (defvar my/fixed-width-font "JetBrains Mono"
       "The font to use for monospaced (fixed width) text.")

       (defvar my/variable-width-font "Inter"
       "The font to use for variable-pitch (document) text.")

; NOTE: These settings might not be ideal for your machine, tweak them as needed!
       (set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 105)
       (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 110)
       (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.1)

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(use-package org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; Org Present --------------------------------------------

;; Install org-present if needed
(unless (package-installed-p 'org-present)
  (package-install 'org-present))

(defun my/org-present-prepare-slide ()
  (org-overview) ;; Show only top-level headlines
  (org-fold-show-entry) ;; Unfold the current entry
  (org-fold-show-children) ;; Show only direct subheadings of the slide but don't expand them
  )

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)
  )

(add-hook 'org-mode-hook
  (lambda ()
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)
  )

  ;; Stop centering the document
  ;; (visual-fill-column-mode 0)
  ;; (visual-line-mode 0))

;; Turn on variable pitch fonts in Org Mode buffers
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))
