;;; -*- lexical-binding: t -*-

(defun sj-create-minor-mode(name)
  (interactive "sEnter name of new minor mode:")
  (let* ((filename (concat name ".el"))
        (full-path (concat "~/.emacs.d/modes/" filename)))
  (with-temp-file full-path
    (insert ";;; -*- lexical-binding: t -*-")
    (dotimes (_ 5) (newline))
    (insert (concat ";;; " filename " ends here")))))

(defun sj-delete()
  "If on whitespace, deletes it. Otherwise, deletes backward-word in vim-like fashion"
  (interactive)
  ;; in evil-insert-mode, the point is shifted over one to the right
  ;; therefore, to see if we're on top of whitespace, we'll temporarily shift left, check and shift right.
  (backward-char 1)
  (let ((on-whitespace (looking-at-p "[[:space:]]")))
    (forward-char 1)
    (if on-whitespace
        (delete-horizontal-space)
      (evil-delete-backward-word))))

