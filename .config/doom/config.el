;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Orhan Berkay Yılmaz"
      user-mail-address "orhanberkayyilmaz@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; (setq doom-theme 'catppuccin)
;; (setq doom-theme 'everblush)

;; (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha

(setq doom-theme 'doom-gruvbox)

;; (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha

;; (catppuccin-reload)
;; (setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! org
        (setq org-roam-directory "~/Documents/org-roam/")
        (setq org-roam-index-file "~/Documents/org-roam/index.org")
        (setq org-startup-folded t))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         A hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-l" . 'copilot-accept-completion)
              ;;("<tab>" . 'copilot-accept-completion)
              ;;("TAB" . 'copilot-accept-completion)
              ;;("C-TAB" . 'copilot-accept-completion-by-word)
              ;;("C-<tab>" . 'copilot-accept-completion-by-word)
              ))

;(add-to-list 'default-frame-alist '(undecorated . t))
;(add-to-list 'default-frame-alist '(alpha . 99))

(after! doom-themes
  ;; set  your favorite themes
  (setq! auto-dark-dark-theme 'doom-gruvbox
        auto-dark-light-theme 'doom-gruvbox-light)
  (auto-dark-mode 1))

;; Let the desktop background show through
(set-frame-parameter (selected-frame) 'alpha '(97 . 99))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;;; Theme and Fonts ----------------------------------------

       ;; Set reusable font name variables
       (defvar my/fixed-width-font "JetBrains Mono Nerd Font"
       "The font to use for monospaced (fixed width) text.")

       (defvar my/variable-width-font "Inter"
       "The font to use for variable-pitch (document) text.")

; NOTE: These settings might not be ideal for your machine, tweak them as needed!
       (set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 110)
       (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 120)
       (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.1)

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

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

;;; Centering Org Documents --------------------------------

;; Install visual-fill-column
(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

;; Configure fill width
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)

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
