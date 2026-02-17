;;; -*- lexical-binding: t; -*-

(use-package evil
  :after pulse
  :config
  (setq pulse-flag t)
  (defun my/evil-pulse-block (beg end)
    "Highlight an Evil blockwise yank correctly (all rows)."
    (let* ((beg-pos (min beg end))
           (end-pos (max beg end))
           (start-col (save-excursion (goto-char beg) (current-column)))
           (end-col   (save-excursion (goto-char end) (current-column)))
           (left  (min start-col end-col))
           (right (max start-col end-col))
           (overlays '()))
      (save-excursion
        (goto-char beg-pos)
        (while (<= (point) end-pos)
          (move-to-column left)
          (let ((row-beg (point)))
            (move-to-column right t)
            (let ((ov (make-overlay row-beg (point))))
              (overlay-put ov 'face 'pulse-highlight-face)
              (push ov overlays)))
          (forward-line 1)))
      ;; Cleanup after pulse-delay
      (run-with-timer
       pulse-delay nil
       (lambda (ovs)
         (mapc #'delete-overlay ovs))
       overlays)))

  (defun my/evil-yank-pulse (orig-fn beg end type register yank-handler)
    (let ((result (funcall orig-fn beg end type register yank-handler)))
      (run-with-timer
       0 nil
       (lambda ()
         (pcase type
           ('block
            (my/evil-pulse-block beg end))
           (_
            (pulse-momentary-highlight-region beg end)))))
      result))

  (advice-add 'evil-yank :around #'my/evil-yank-pulse))

(provide 'my)
