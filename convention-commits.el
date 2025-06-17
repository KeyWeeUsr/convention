;;; convention-commits.el --- Commits syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defsubst convention-commits-syntax-defaults ()
  "Basic setup for font-lock."
  (setq-local font-lock-keywords-case-fold-search t))

(defsubst convention-commits-syntax-keywords ()
  "Create conventional comments keywords."
  (let ((plain
         (rx-to-string `(and
          string-start (group (or ,@convention-commits-keywords)) (*? not-newline)
          (group (? (literal "!"))) (group (literal ":"))
          (group (*? not-newline))
          line-end)))
        (decorated
         (rx-to-string `(and
          string-start (or ,@convention-commits-keywords) (*? not-newline)
          (group (literal "(")) (*? not-newline) (group (literal ")"))
          (group (? (literal "!"))) (group (literal ":"))
          (group (*? not-newline)) line-end)))
        (decoration-anchor
         (rx-to-string `(and
          string-start (or ,@convention-commits-keywords) (*? whitespace)
          (literal "(") (group (*? anychar)) (literal ")") (? (literal "!"))
          (literal ":"))))
        (decoration (rx (+? (group (+ not-newline)) (*? (literal ","))))))
    `((,plain
       (1 'font-lock-keyword-face t)
       (2 '(bold italic underline :foreground "#ff0000") t)
       (3 'bold t)
       (4 'italic append))
      (,decorated
       (1 'bold t)
       (2 'bold t)
       (3 '(bold italic underline :foreground "#ff0000") t)
       (4 'bold t)
       (5 'italic append))
      (,decoration-anchor
       (,decoration
        ;; pre-match form
        (progn
          ;; start matching in the parenthesis
          (goto-char (match-beginning 1))
          (let* ((begin (match-beginning 1))
                 (end (match-end 1))
                 (sub (buffer-substring-no-properties
                       begin end))
                 (colors convention-commits-decoration-colors)
                 (colors-len (length colors))
                 (last begin)
                 (idx 0))
            (while (re-search-forward "," end t)
              (let ((prop `(:foreground ,(nth (mod idx colors-len) colors))))
                (put-text-property last (match-beginning 0) 'face prop))
              (setq last (1+ (match-beginning 0)))
              (setq idx (1+ idx)))
            (when (< last end)
              (let ((prop `(:foreground ,(nth (mod idx colors-len) colors))))
                (put-text-property last end 'face prop))))

          ;; then keep the anchored match loop within the block
          (match-end 0))
        ;; post-match form
        nil
        ;; no group-matching props needed
        )))))

(defun convention-commits-syntax--activate ()
  "Add conventional comments syntax."
  (convention-commits-syntax-defaults)
  (font-lock-add-keywords nil (convention-commits-syntax-keywords))
  (font-lock-update))

(defun convention-commits-syntax--deactivate ()
  "Remove conventional comments syntax."
  (convention-commits-syntax-defaults)
  (font-lock-remove-keywords nil (convention-commits-syntax-keywords))
  (font-lock-update))


(provide 'convention-commits)
;;; convention-commits.el ends here
