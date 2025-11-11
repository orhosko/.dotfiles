;; ---------------------------------------------------------------------------

;; A quick primer on the `use-package' function (refer to
;; "C-h f use-package" for the full details).
;;
;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :init        ; Run this code before my-package is loaded
;;   :bind        ; Bind these keys to these functions
;;   :hook        ; Add hooks
;;   :custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded
;; )

;; ---------------------------------------------------------------------

(load (expand-file-name "packages/llms.el" user-emacs-directory))

;; ---------------------------------------------------------------------

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ; (customize-set-variable 'evil-respect-visual-line-mode t)
  (customize-set-variable 'evil-want-C-h-delete t)
  (customize-set-variable'evil-want-C-u-scroll t)
  :config
  (setq evil-kill-on-visual-paste nil)
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(keymap-set evil-insert-state-map "C-g" 'evil-normal-state)
(keymap-global-set "C-M-u" 'universal-argument)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :ensure t
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)
)

(use-package multiple-cursors
  :ensure t
  :bind(("C-S-c C-S-c" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        ("C-c m n" . mc/insert-numbers))
  :config
  (setq mc/insert-numbers-default 1)
)

;; ---------------------------------------------------------------------------

(use-package copilot
  :ensure t
  :vc (:fetcher github :repo copilot-emacs/copilot.el)
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-l" . 'copilot-accept-completion)
              ;;("<tab>" . 'copilot-accept-completion)
              ;;("TAB" . 'copilot-accept-completion)
              ;;("C-TAB" . 'copilot-accept-completion-by-word)
              ;;("C-<tab>" . 'copilot-accept-completion-by-word)
              )
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(c++-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

;; ---------------------------------------------------------------------------

(setq completion-auto-help nil)

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; ---------------------------------------------------------------------------

;;; Extended completion utilities
(use-package consult
  :ensure t
  :config
  (setq consult-preview-key nil)
  (setq completion-auto-help nil)) ;; Prevents extra *Completions* buffer
;; (global-set-key [rebind switch-to-buffer] #'consult-buffer)
;; (global-set-key (kbd "C-c j") #'consult-line)
;; (global-set-key (kbd "C-c i") #'consult-imenu)
;; (setq read-buffer-completion-ignore-case t
;;      read-file-name-completion-ignore-case t
;;      completion-ignore-case t))

;; ---------------------------------------------------------------------------

;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  :config
  (setq text-mode-ispell-word-completion nil)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  ;; (corfu-auto-delay 0)
  ;; (corfu-auto-prefix 0)
  ;; (completion-styles '(basic))
  )

;; ---------------------------------------------------------------------------

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :config
  (setq magit-diff-refine-hunk t) ;; Show word-granularity differences within diff hunks
  )

;; ---------------------------------------------------------------------------

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (setq-local flymake-start-on-flymake-mode nil) ;; Don't start flymake automatically
            (setq-local flymake-start-on-save-buffer nil) ;; Don't start flymake on save
            (flymake-mode 1))) ;; Enable flymake mode

;;; DAP Support
(use-package dape
  :ensure t
  :config
  (setq dape-inlay-hints t) ;; Add hints for variable values in buffer
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line) ;; Emphasize currently source line of current debugged program
  ;;(defalias 'start-debugging #'dape)
 )

;;; Indication of local VCS changes
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  ;; (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
)

(use-package dtrt-indent
  :ensure t
  :hook (prog-mode . dtrt-indent-mode))

(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of using vc
  ;:vc (:repo "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package crdt
  :ensure t)

(use-package tabspaces
  :ensure t
  :config
  (setq tab-bar-select-tab-modifiers '(meta))
  (tabspaces-mode)
  (define-key evil-normal-state-map (kbd "SPC TAB ,") 'tabspaces-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC TAB b") 'tabspaces-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC TAB k") 'tabspaces-kill-buffers-close-workspace)
  (define-key evil-normal-state-map (kbd "SPC TAB o") 'tabspaces-open-or-create-project-and-workspace)
  (define-key evil-normal-state-map (kbd "SPC TAB TAB") 'tabspaces-switch-or-create-workspace)
  (customize-set-variable 'tabspaces-use-filtered-buffers-as-default t)
  (customize-set-variable 'tabspaces-remove-to-default t)
  (customize-set-variable 'tabspaces-include-buffers '("*scratch*")))

;;; EditorConfig support
;; (unless (package-installed-p 'editorconfig)
  ;; (package-install 'editorconfig))

;; Enable EditorConfig
;; (editorconfig-mode t)

;; (use-package projectile
;;  :ensure t
;;  :config
;;  (projectile-mode 1)
;;  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

; (use-package flycheck-posframe
;   :ensure t
;   :after flycheck
;   :config
;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;   (flycheck-posframe-configure-pretty-defaults))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-list '((?+ . "➕") (?* . "➤") (?- . "➖"))))

(modify-all-frames-parameters
 '((right-divider-width . 20)
   (internal-border-width . 20)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(global-org-modern-mode)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; ---------------------------------------------------------------------------

(use-package aider
    :ensure t
    :config
    (setq aider-args '("--model" "gemini" "--no-auto-accept-architect" "--no-auto-commits"))
    (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
    ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
    (aider-magit-setup-transients) ;; add aider magit function to magit menu
    (global-auto-revert-mode 1) ;; auto revert buffer
    (auto-revert-mode 1))

;; ---------------------------------------------------------------------------

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

;; ---------------------------------------------------------------------------
;;; Language modes
;; ---------------------------------------------------------------------------

(use-package go-mode
  :after eglot
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package typescript-mode
  :after eglot
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'reftex-mode))

;; ---------------------------------------------------------------------------

(use-package verilog-ext
  :ensure t
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET verilog-ext':
  ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          lsp
          flycheck
          beautify
          navigation
          template
          formatter
          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
          ports))
  :config
  (verilog-ext-mode-setup)
  (define-key verilog-mode-map (kbd ";") 'self-insert-command))

(use-package org-download
  :ensure t)
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package ob-mermaid
  :ensure t
  :after org
  :config
  (setq ob-mermaid-cli-path "mmdc")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (scheme . t)
     ;(your-other-langs . t)
     ))
  )

;; TODO: find a way to both support c and c++ modes
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
