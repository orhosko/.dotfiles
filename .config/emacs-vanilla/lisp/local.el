;;; -*- lexical-binding: t; -*-

(setenv "PATH" (concat (getenv "PATH") ":/home/berkay/.local/bin:/home/berkay/bin"))
(add-to-list 'exec-path "/home/berkay/.local/bin")
(add-to-list 'exec-path "/home/berkay/bin")

(setenv "DICPATH" "/home/berkay/.config/hunspell")

;; ---------------------------------------------------------------------

(use-package pdf-tools
  :ensure t
  :config
  (require 'pdf-occur)
  (pdf-tools-install))

;; ---------------------------------------------------------------------

(use-package dired-aux
  :ensure nil
  :config
  (add-to-list 'dired-compress-file-alist
               '("\\.7z\\'" . "7z a -mx=9 %o %i"))

  (add-to-list 'dired-compress-files-alist
               '("\\.7z\\'" . "7z a -mx=9 %o %i"))
  ;; (setq dired-compress-directory-default-suffix ".7z")
  )

(provide 'local)
