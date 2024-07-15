(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("78802391300fb87ea01aa7e2d7310f9564cf83aa97d8f1ecf02b9b400375abd7" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "77364a6e41e64138e2c9beb11a40ddfe73551587e86251afdc728edaf51931d9" "276c08753eae8e13d7c4f851432b627af58597f2d57b09f790cb782f6f043966" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" default))
 '(doc-view-continuous t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(catppuccin-theme treemacs auto-dark))
 '(safe-local-variable-values
   '((eval setq org-image-actual-width 600)
     (eval add-hook 'after-save-hook
      (lambda nil
        (org-html-export-to-html t))
      t t)
     (eval add-hook 'before-change-functions
      (lambda
        (beg end)
        (save-excursion
          (goto-char beg)
          (while
              (search-forward "▶" end t)
            (replace-match "-" nil t))))
      nil t)
     (eval add-hook 'after-save-hook 'org-html-export-to-html t t)))
 '(warning-suppress-types
   '((doom-first-file-hook)
     (doom-first-file-hook)
     (doom-first-file-hook)
     (doom-first-file-hook)
     (doom-first-file-hook)
     (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
