;;; -*- lexical-binding: t; -*-

;; --- Sane settings ----------------------------------------------------------

(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

(set-default-coding-systems 'utf-8)

(setq-default ring-bell-function 'ignore)
(setq-default select-enable-clipboard t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 120)

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq use-dialog-box nil)

(show-paren-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(setq confirm-kill-emacs #'y-or-n-p)
(setopt use-short-answers t)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; ---------------------------------------------------------------------------

(setq default-frame-alist '((undecorated . t)))
;; (set-frame-parameter (selected-frame) 'alpha '(98 . 95))

(blink-cursor-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; (pixel-scroll-precision-mode 1)
;; (icomplete-vertical-mode 1)

;; ---------------------------------------------------------------------------

(setq font-use-system-font t) ;; Use whatever the default monospace font is

;; Set the font. Note: height = px * 100
;; (set-face-attribute 'default nil :font "JetBrains Mono" 
;; 				:height 105 :weight 'light) 

;; (set-face-attribute 'default nil
;;                    :height 140 :weight 'light :family "Roboto Mono")
;; (set-face-attribute 'bold nil :weight 'regular)
;; (set-face-attribute 'bold-italic nil :weight 'regular)
;; (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
;; (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; ---------------------------------------------------------------------------

;; (load-theme 'wombat t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-dark-theme 'doom-gruvbox)
  (setq auto-dark-light-theme 'doom-one-light)
  (auto-dark-mode 1))

;; ---------------------------------------------------------------------------

(provide 'ui)
