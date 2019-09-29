;;; -*- lexical-binding: t -*-

(defvar ppm-was-enabled prettify-symbols-mode)

(define-minor-mode pop-pretty-mode
  "temporarily disables pretty-mode - python slows down when rendering prettified logical operands"
  :lighter " ppm"
  (if pop-pretty-mode
    (prettify-symbols-mode -1)
    (when ppm-was-enabled
      (prettify-symbols-mode t))))

;;; pop-pretty.el ends here
