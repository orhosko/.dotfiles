(setq native-comp-async-report-warnings-errors nil)

;; ---------------------------------------------------------------------------

;; Bring in package utilities so we can install packages from the web.
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(add-to-list 'load-path package-user-dir)
(when (file-directory-p package-user-dir)
  (let ((default-directory package-user-dir))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add MELPA, an unofficial (but well-curated) package registry to the
;; list of accepted package registries. By default Emacs only uses GNU
;; ELPA and NonGNU ELPA, https://elpa.gnu.org/ and
;; https://elpa.nongnu.org/ respectively.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun berkay/package-ensure-autoloads ()
  "Generate missing package autoloads in `package-user-dir'."
  (when (file-directory-p package-user-dir)
    (dolist (dir (directory-files package-user-dir t "\\`[^.]") )
      (when (file-directory-p dir)
        (let ((pkg-desc (ignore-errors (package--read-pkg-desc dir))))
          (when pkg-desc
            (let ((autoload-file (expand-file-name
                                  (format "%s-autoloads.el" (package-desc-name pkg-desc))
                                  dir)))
              (unless (file-exists-p autoload-file)
                (package-generate-autoloads (package-desc-name pkg-desc) dir)))))))))

(berkay/package-ensure-autoloads)
(package-initialize)

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
  (add-to-list 'eglot-server-programs
               `(mlir-mode . ("mlir-lsp-server")))
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

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(setq xref-search-program 'ripgrep)

;; (electric-pair-mode t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(when (cdr command-line-args)
  (setq inhibit-startup-screen t))

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

;; ---------------------------------------------------------------------------

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; ---------------------------------------------------------------------------

(setq user-full-name "Orhan Berkay Yılmaz"
      user-mail-address "yilmazorhanberkay@gmail.com")

;; ---------------------------------------------------------------------------

;; Enable .dir-locals.el variables for remote files
(setq enable-remote-dir-locals t)

;; Don't make project.el search submodules for project roots.
(setq project-vc-merge-submodules nil)

;; ---------------------------------------------------------------------------

;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)

(setenv "PATH" (concat (getenv "PATH") ":/home/berkay/.local/bin:/home/berkay/bin"))
(add-to-list 'exec-path "/home/berkay/.local/bin")
(add-to-list 'exec-path "/home/berkay/bin")

(setenv "DICPATH" "/home/berkay/.config/hunspell")

(require 'ui)
(require 'packages)
(require 'org-conf)
(require 'present)
(require 'keybindings)
(require 'scroll)
(require 'my)
(require 'clipboard-tty)
; (load (expand-file-name "modes/mlir-mode.el" user-emacs-directory))
