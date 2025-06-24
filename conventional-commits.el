;;; conventional-commits.el --- Commits syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'conventional-custom)

(defsubst conventional-commits-syntax-keywords ()
  "Create conventional comments keywords."
  (let ((plain
         (rx-to-string `(and
          string-start (group (or ,@conventional-commits-keywords))
          (*? not-newline)
          (group (? (literal "!"))) (group (literal ":"))
          (group (*? not-newline))
          line-end)))
        (decorated
         (rx-to-string `(and
          string-start (or ,@conventional-commits-keywords) (*? not-newline)
          (group (literal "(")) (*? not-newline) (group (literal ")"))
          (group (? (literal "!"))) (group (literal ":"))
          (group (*? not-newline)) line-end)))
        (decoration-anchor
         (rx-to-string `(and
          string-start (or ,@conventional-commits-keywords) (*? whitespace)
          (literal "(") (group (*? anychar)) (literal ")") (? (literal "!"))
          (literal ":"))))
        (decoration (rx (+? (group (+ not-newline)) (*? (literal ",")))))
        (footer (rx (or (literal "\n\n") (literal "\r\n\r\n"))
                    (group (+ (+ not-newline) "\n"))
                    string-end))
        (footer-header
         (rx line-start
             (or (group "breaking change")
                 (group (+ (and (+ word) (? (literal "-"))))))
             (group (literal ":"))
             (group (*? not-newline))
             line-end)))
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
                 (colors conventional-commits-decoration-colors)
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
        ))
      (,footer
       (,footer-header
        ;; pre-match form
        (progn
          (put-text-property (match-beginning 0)
                             (match-end 0)
                             'font-lock-multiline t)
          (goto-char (match-beginning 1))
          ;; then keep the anchored match loop within the block
          (match-end 1))
        ;; post-match form
        nil
        ;; no group-matching props needed
        (1 '(bold italic :foreground "#ff0000") nil t)
        (2 'font-lock-keyword-face nil t)
        (3 'bold)
        (4 'italic))))))

(defun conventional-commits--should-suggest ()
  "Check COMMIT_EDITMSG whether it's empty."
  (catch 'suggest
    (dolist (line (split-string (buffer-string) "\n" t " "))
      (unless (string-prefix-p "#" line)
        (throw 'suggest nil)))
    (throw 'suggest t)))

(defun conventional-commits--ask-type ()
  "Ask user for commit type to insert."
  (let* ((start ?0) (prompt "Available choices:\n\n")
         choices choices-keys chosen)
    (dotimes (idx (length conventional-commits-keywords))
      (let ((num (+ start idx))
            (val (nth idx conventional-commits-keywords)))
        (push num choices-keys)
        (setf (alist-get (intern (number-to-string num)) choices) val)
        (setq prompt (format "%s%c = %s\n" prompt num val))))
    (setq prompt (format "%s\n\nC-g = Quit" prompt))
    (setq chosen (read-char-choice prompt (reverse choices-keys)))
    (let ((found (alist-get (intern (number-to-string chosen)) choices)))
      (when found
        (insert (format "%s: " found))))))

(defun conventional-commits-syntax--activate ()
  "Add conventional comments syntax."
  (when (and conventional-commits-ask-for-type
             (conventional-commits--should-suggest))
    (save-window-excursion
      (conventional-commits--ask-type)))

  ;; todo(font-lock): whenever font-lock-defaults / set-defaults is used
  ;; jit-lock-mode is not applied and mangles the highlighting
  ;; and the hook is missing too
  (jit-lock-mode 1)
  (add-hook 'after-change-functions #'font-lock-after-change-function t t)

  (setq-local font-lock-defaults '(nil nil t))
  (font-lock-add-keywords nil (conventional-commits-syntax-keywords))
  (font-lock-update))

(defun conventional-commits-syntax--deactivate ()
  "Remove conventional comments syntax."
  (remove-hook 'after-change-functions #'font-lock-after-change-function t)
  (setq-local font-lock-defaults '(nil nil t))
  (font-lock-remove-keywords nil (conventional-commits-syntax-keywords))
  (font-lock-update))


(provide 'conventional-commits)
;;; conventional-commits.el ends here
