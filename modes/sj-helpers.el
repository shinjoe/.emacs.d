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

(defun sj-toggle-comments()
  "intelligently toggles comments based on context"
  (interactive)
  ;; three main cases:
  ;; 1) if on a left paren, we'll comment out the underlying symbolic expression
  ;; 2) if visual chunk selected, we'll comment out all the selected lines
  ;; 3) if leftward comments are detected, comments will be stripped
  (save-excursion
    (let ((beg (point))
          (on-left-paren (looking-at-p "("))
          (in-visual-chunk (string= evil-state "visual"))
          (in-comment-section nil))
      (cond (on-left-paren (comment-region beg (progn (mark-sexp) (goto-char (mark)))))
            (in-visual-chunk (comment-or-uncomment-region
                              (save-excursion (goto-char (region-beginning))(line-beginning-position))
                              (save-excursion (goto-char (region-end))(line-end-position))))
            (t "not impl yet :)")))))

