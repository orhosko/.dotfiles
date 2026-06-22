;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setopt user-full-name "Orhan Berkay Yılmaz"
      user-mail-address "yilmazorhanberkay@gmail.com")

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
;;(setopt doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
;; (setopt catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
;; (catppuccin-reload)
;;
;; (setopt doom-theme 'doom-one)
;; (setopt doom-theme 'everblush)
(setopt doom-theme 'doom-gruvbox)

;; (after! catppuccin-theme
;;   (setopt catppuccin-flavor 'latte))

;; (after! auto-dark
;;   (setopt auto-dark-dark-theme 'doom-gruvbox
;;           auto-dark-light-theme 'catppuccin)
;;   (auto-dark-mode 1))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setopt display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setopt org-directory "~/Documents/org/")

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
        (setopt org-roam-directory "~/Documents/org-roam/")
        (setopt org-roam-index-file "~/Documents/org-roam/index.org")
        (setopt org-startup-folded 'fold))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         A hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setopt org-roam-ui-sync-theme t
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

(add-to-list 'default-frame-alist '(undecorated . t))
;(set-frame-parameter (selected-frame) 'alpha '(97 . 99))

;; Let the desktop background show through
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Configure fill width
(after! visual-fill-column
(setopt visual-fill-column-width 160
      visual-fill-column-center-text t))

;;; Org Present --------------------------------------------

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
  (org-link-preview-region)
  )

(add-hook 'org-mode-hook
  (lambda ()
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
(add-hook! 'org-present-mode-hook 'my/org-present-start)
(add-hook! 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook! 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'display-line-numbers-equalize)

(use-package! org-appear
   :hook (org-mode . org-appear-mode))

;; (require 'verilog-ext)
;; (verilog-ext-mode-setup)
;; (verilog-ext-eglot-set-server 've-verible-ls)

;; (use-package! lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log nil)
;;   (global-lsp-bridge-mode))

;; Performance tweaks for modern machines
(setopt gc-cons-threshold 100000000) ; 100 mb
(setopt read-process-output-max (* 1024 1024)) ; 1mb

(setopt +corfu-want-tab-prefer-navigating-snippets t)

(use-package! ispell
  :init
  (setopt ispell-program-name "hunspell")
  (setopt ispell-dictionary "en_US,tr_TR")

  :config
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,tr_TR")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setopt ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;; TODO(orhosko): broken with tramp
;; (use-package! xclip
;;   :if (and (not (display-graphic-p)) (getenv "DISPLAY"))
;;   :config (xclip-mode 1))

;; (when (and (not (display-graphic-p)) (getenv "WAYLAND_DISPLAY")
;;            (executable-find "wl-copy") (executable-find "wl-paste"))
;;   (defun my/wl-copy (text &optional _push)
;;     (let ((p (make-process :name "wl-copy" :buffer nil
;;                            :command '("wl-copy" "--type" "text/plain;charset=utf-8")
;;                            :connection-type 'pipe)))
;;       (when text (process-send-string p text) (process-send-eof p))))
;;   (defun my/wl-paste ()
;;     (let ((text (shell-command-to-string "wl-paste --no-newline --type text/plain")))
;;       (unless (string= text "") text)))
;;   (setq interprogram-cut-function   #'my/wl-copy
;;         interprogram-paste-function #'my/wl-paste))

;; TODO(orhosko): broken
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
