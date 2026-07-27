;;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :hook prog-mode
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


;; (use-package gptel
;;   :ensure t
;;   ;:init        ; Run this code before my-package is loaded
;;   ;:bind        ; Bind these keys to these functions
;;   ;:custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded
;;   (setq gptel-model 'gemini-2.0-flash-exp)
;;   (setq gptel-backend
;;         (gptel-make-gemini "Gemini"
;;                            :key (getenv "GEMINI_API_KEY")
;;                            :stream t))
;; )

;; (use-package aider
;;     :ensure t
;;     :config
;;     ;; (setq aider-args '("--model" "gpt-5-codex" "--no-auto-accept-architect" "--no-auto-commits")) :: use .aider.config.yaml instead
;;     (global-set-key (kbd "C-c a") 'aider-transient-menu) ;; for wider screen
;;     ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
;;     (aider-magit-setup-transients) ;; add aider magit function to magit menu
;;     (global-auto-revert-mode 1) ;; auto revert buffer
;;     (auto-revert-mode 1))

; TODO: try ellama

(provide 'llms)
