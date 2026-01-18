; TODO: maybe try aider.el and ellama

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
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


(use-package gptel
  :ensure t
  ;:init        ; Run this code before my-package is loaded
  ;:bind        ; Bind these keys to these functions
  ;:custom      ; Set these variables
  :config      ; Run this code after my-package is loaded
  (setq gptel-model 'gemini-2.0-flash-exp)
  (setq gptel-backend
        (gptel-make-gemini "Gemini"
                           :key (getenv "GEMINI_API_KEY")
                           :stream t))
)
