(setq init-start-time (current-time))

;; ---------------------------------------------------------------------------

(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; --- Activate / Deactivate modes --------------------------------------------

;; (icomplete-vertical-mode 1)

(tool-bar-mode -1) (blink-cursor-mode -1)
(scroll-bar-mode -1) (menu-bar-mode -1) 
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)

;; --- Sane settings ----------------------------------------------------------

(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t)

;; ---------------------------------------------------------------------------

;; (setq font-use-system-font t) ;; Use whatever the default monospace font is

;; Set the font. Note: height = px * 100
(set-face-attribute 'default nil :font "JetBrains Mono Nerd Font" 
				:height 120 :weight 'light) 

;; (set-face-attribute 'default nil
;;                    :height 140 :weight 'light :family "Roboto Mono")
;; (set-face-attribute 'bold nil :weight 'regular)
;; (set-face-attribute 'bold-italic nil :weight 'regular)
;; (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
;; (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; ---------------------------------------------------------------------------

;; (load-theme 'wombat t)

;; ---------------------------------------------------------------------------

;; Bring in package utilities so we can install packages from the web.
(require 'package)

;; Add MELPA, an unofficial (but well-curated) package registry to the
;; list of accepted package registries. By default Emacs only uses GNU
;; ELPA and NonGNU ELPA, https://elpa.gnu.org/ and
;; https://elpa.nongnu.org/ respectively.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Add the :vc keyword to use-package, making it easy to install
;; packages directly from git repositories.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; ---------------------------------------------------------------------------

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)

;; ;; The `setq' special form is used for setting variables. Remember
;; ;; that you can look up these variables with "C-h v variable-name".
(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      ;; I'll add an extra note here since user customizations are important.
      ;; Emacs actually offers a UI-based customization menu, "M-x customize".
      ;; You can use this menu to change variable values across Emacs. By default,
      ;; changing a variable will write to your init.el automatically, mixing
      ;; your hand-written Emacs Lisp with automatically-generated Lisp from the
      ;; customize menu. The following setting instead writes customizations to a
      ;; separate file, custom.el, to keep your init.el clean.
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation))
  :config
  (add-hook 'prog-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
               `(verilog-mode . ("verible-verilog-ls")))
  )

;; In addition to installing packages from the configured package
;; registries, you can also install straight from version control
;; with the :vc keyword argument. For the full list of supported
;; fetchers, view the documentation for the variable
;; `vc-use-package-fetchers'.
;;
;; Breadcrumb adds, well, breadcrumbs to the top of your open buffers
;; and works great with project.el, the Emacs project manager.
;;
;; Read more about projects here:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
(use-package breadcrumb
  :vc (:fetcher github :repo joaotavora/breadcrumb)
  :init (breadcrumb-mode))

;; ------------------------------------------------------------------

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq use-dialog-box nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(electric-pair-mode t)
(show-paren-mode 1)

(setq confirm-kill-emacs #'y-or-n-p)
(setopt use-short-answers t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(when (cdr command-line-args)
  (setq inhibit-startup-screen t))

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

;; ---------------------------------------------------------------------------

(load (expand-file-name "+packages.el" user-emacs-directory))
(load (expand-file-name "+keybindings.el" user-emacs-directory))

; (load (expand-file-name "+nano-theme.el" user-emacs-directory))
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t))

; (load "~/nano-emacs/nano-modeline.el")
(load (expand-file-name "+present.el" user-emacs-directory))
(load (expand-file-name "+org.el" user-emacs-directory))

;; ---------------------------------------------------------------------------

(setq user-full-name "Orhan Berkay Yılmaz"
      user-mail-address "yilmazorhanberkay@gmail.com")

;; ---------------------------------------------------------------------------

;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)

(set-frame-parameter (selected-frame) 'alpha '(99 . 96))

(setenv "PATH" (concat (getenv "PATH") ":/home/berkay/.local/bin:/home/berkay/bin"))
(add-to-list 'exec-path "/home/berkay/.local/bin")
(add-to-list 'exec-path "/home/berkay/bin")

;; --- Speed benchmarking -----------------------------------------------------

(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))
