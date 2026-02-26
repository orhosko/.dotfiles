;;; -*- lexical-binding: t; -*-

(defun my/kill-new-to-system-clipboard (&rest _)
  (let ((text (current-kill 0 t)))
    (send-string-to-terminal
     (format "\033]52;c;%s\a"
             (base64-encode-string
              (encode-coding-string text 'utf-8) t)))))

(when (getenv "TMUX")
  (advice-add 'kill-new :after #'my/kill-new-to-system-clipboard))

(provide 'tmux)
