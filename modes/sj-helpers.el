;;; -*- lexical-binding: t -*-

(defun sj-create-minor-mode(name)
  (interactive "sEnter name of new minor mode:")
  (let* ((filename (concat name ".el"))
        (full-path (concat "~/.emacs.d/modes/" filename)))
  (with-temp-file full-path
    (insert ";;; -*- lexical-binding: t -*-")
    (dotimes (_ 5) (newline))
    (insert (concat ";;; " filename " ends here")))))

