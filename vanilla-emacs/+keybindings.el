; a unified keybindings(doom inspired):
; - SPC leader
;  - belki emacs mode kullanırssak bir gün SPC ve SPC-m yerine C-x(leader) ve C-c(localleader)
; - Ctrl

; - spc-b
;   - +buffer
;   - b: switch buffer
;   - d: delete buffer
;   - D: delete this buffer
;   - r: revert buffer
(define-key evil-normal-state-map (kbd "SPC b b") 'switch-to-buffer) ;; +vertico/switch-workspace-buffer
(define-key evil-normal-state-map (kbd "SPC b d") 'kill-buffer) ;; +vertico/kill-buffer)
(define-key evil-normal-state-map (kbd "SPC b D") (lambda () (interactive) (kill-buffer (current-buffer))))
(define-key evil-normal-state-map (kbd "SPC b r") (lambda () (interactive) (revert-buffer nil t)))

; - SPC-SPC
;   - find files in project
;   - quick switch
(define-key evil-normal-state-map (kbd "SPC SPC") 'project-find-file)

; - SPC-n
;   - +notes
;   - SPC-n-r
;     - roam
(defun org-find ()
  (interactive)
  (find-file org-directory))

(define-key evil-normal-state-map (kbd "SPC n r f") 'org-roam-node-find)
(define-key evil-normal-state-map (kbd "SPC n r i") 'org-roam-node-insert)
(define-key evil-normal-state-map (kbd "SPC n f") 'org-find)
(define-key evil-normal-state-map (kbd "SPC n c") 'org-capture)
(define-key evil-normal-state-map (kbd "SPC n a") 'org-agenda)

; - SPC-.
;   - find files
;   - +create files
(define-key evil-normal-state-map (kbd "SPC .") 'find-file)

; - SPC-,
;   - switch buffer: SPC-b-b
(define-key evil-normal-state-map (kbd "SPC ,") 'switch-to-buffer)

; - SPC-w
;   - +window
;   - v: vertical split
;   - s: "sunset" split
;   - hjkl: move focus
;   - HJKL: move window
(define-key evil-normal-state-map (kbd "SPC w v") 'evil-window-vsplit)
(define-key evil-normal-state-map (kbd "SPC w s") 'evil-window-split)
(define-key evil-normal-state-map (kbd "SPC w h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "SPC w j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "SPC w k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "SPC w l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "SPC w H") 'evil-window-move-far-left)
(define-key evil-normal-state-map (kbd "SPC w J") 'evil-window-move-very-bottom)
(define-key evil-normal-state-map (kbd "SPC w K") 'evil-window-move-very-top)
(define-key evil-normal-state-map (kbd "SPC w L") 'evil-window-move-far-right)

; - spc-t
;   - +toggle

; - SPC-p
;   - +project
;   - .: search in project files
;   - b: switch buffer
;   - c: compile project 
;   - d: find-dir
;   - D: dired
;   - p: switch project
(define-key evil-normal-state-map (kbd "SPC p .") 'project-find-file)
(define-key evil-normal-state-map (kbd "SPC p p") 'project-switch-project)
(define-key evil-normal-state-map (kbd "SPC p b") 'project-switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC p c") 'project-compile)
(define-key evil-normal-state-map (kbd "SPC p d") 'project-dired)
(define-key evil-normal-state-map (kbd "SPC p D") 'project-find-dir)
(define-key evil-normal-state-map (kbd "SPC p g") 'project-find-regexp)

; - SPC-s
;   - +search

; - SPC-g?
;   - magit
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)

; - spc-m
;   - localleader: mode a özel

; - SPC-TAB
;   - workspace

; - SPC-c
;   - +code/lsp
;   - a: action
;   - r: rename
;   - n: next error
;   - p: previous error
;   - f: format
;   - ---------
;   - c: compile
;   - ?C: recompile or compile at root
(define-key evil-normal-state-map (kbd "SPC c a") 'eglot-code-action-quickfix)
(define-key evil-normal-state-map (kbd "SPC c c") 'compile)
(define-key evil-normal-state-map (kbd "SPC c r") 'eglot-rename)
(define-key evil-normal-state-map (kbd "SPC c n") 'flymake-goto-next-error)
(define-key evil-normal-state-map (kbd "SPC c p") 'flymake-goto-prev-error)
(define-key evil-normal-state-map (kbd "SPC c f") 'eglot-format-buffer)

; - SPC--
;   - dired/filetree
(define-key evil-normal-state-map (kbd "SPC -") 'dired)

; - SPC-o
;   - +open
;   - c: config
;   - a: agenda
;   - s: scratch buffer
;   - -: dired/oil.nvim
;   - t/p?: filetree??
;   - r: REPL???
; - M-<number>
;   - switch workspace
;   - terminalde tab değişmek yeterli nvimde

; - SPC-f
;   - +find
;   - c: config

(defun find-config ()
  (interactive)
  (find-file (file-name-directory user-init-file)))
(define-key evil-normal-state-map (kbd "SPC f c") 'find-config)

;; <visual>gc ;; normally C-x C-;
(define-key evil-visual-state-map "gc" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "gcc" 'comment-line)

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

;; <normal>C-<tab>
(define-key evil-normal-state-map (kbd "C-<tab>") 'toggle-fold)
(define-key evil-normal-state-map (kbd "z a") 'toggle-fold)

;; <normal>SPC-s search
(define-key evil-normal-state-map (kbd "SPC s b") 'consult-line)
(define-key evil-normal-state-map (kbd "SPC s g") 'consult-grep)
(define-key evil-normal-state-map (kbd "SPC s r") 'consult-ripgrep)

;; C-c r consult register 
(global-set-key (kbd "C-c r r") #'consult-register)
(global-set-key (kbd "C-c r s") #'consult-register-store)
