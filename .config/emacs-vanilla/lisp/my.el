;;; -*- lexical-binding: t; -*-

(use-package evil
  :after pulse
  :config
  (setq pulse-flag t)

  (defun my/evil--pos (x)
    "Return numeric position for marker or integer."
    (if (markerp x) (marker-position x) x))

  (defun my/evil-pulse-block (beg end)
    "Highlight an Evil blockwise yank correctly (all rows)."
    (let* ((beg (my/evil--pos beg))
           (end (my/evil--pos end))
           (beg-pos (min beg end))
           (end-pos (max beg end))
           (start-col (save-excursion (goto-char beg) (current-column)))
           (end-col   (save-excursion (goto-char end) (current-column)))
           (left  (min start-col end-col))
           (right (max start-col end-col))
           (overlays nil))
      (save-excursion
        (goto-char beg-pos)
        ;; iterate line by line until we pass end-pos
        (while (<= (point) end-pos)
          (move-to-column left)
          (let ((row-beg (point)))
            (move-to-column right t)
            (let ((ov (make-overlay row-beg (point))))
              (overlay-put ov 'face 'pulse-highlight-face)
              (push ov overlays)))
          (forward-line 1)))
      (run-with-timer
       pulse-delay nil
       (lambda (ovs) (mapc #'delete-overlay ovs))
       overlays)))

  (defun my/evil-yank-pulse (orig-fn &rest args)
    "Pulse region after evil-yank, including blockwise."
    (let* ((beg (nth 0 args))
           (end (nth 1 args))
           (type (nth 2 args))
           (result (apply orig-fn args)))
      (run-with-timer
       0 nil
       (lambda ()
         (pcase type
           ('block
            (my/evil-pulse-block beg end))
           (_
            (pulse-momentary-highlight-region
             (my/evil--pos beg) (my/evil--pos end))))))
      result))

  (advice-add 'evil-yank :around #'my/evil-yank-pulse))

(provide 'my)
