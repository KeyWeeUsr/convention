;;; convention-comments.el --- Comments syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'convention-custom)

(defsubst convention-comments-syntax-defaults ()
  "Basic setup for font-lock."
  (font-lock-set-defaults)
  (setq-local font-lock-keywords-case-fold-search t))

(defsubst convention-comments-syntax-keywords (comment-string)
  "Create conventional comments keywords for ELisp.
Argument COMMENT-STRING represents one or more characters beginning a comment."
  (let ((plain
         (rx-to-string `(and
          (literal ,comment-string) (*? whitespace)
          (group (or ,@convention-comments-keywords))
          (*? not-newline)
          (group (literal ":"))
          (group (*? not-newline))
          line-end)))
        (decorated
         (rx-to-string `(and
          (literal ,comment-string) (*? whitespace)
          (or ,@convention-comments-keywords)
          (*? not-newline)
          (group (literal "("))
          (*? not-newline)
          (group (literal ")"))
          (group (literal ":"))
          (group (*? not-newline))
          line-end)))
        (decoration-anchor
         (rx-to-string `(and
          (literal ,comment-string) (*? whitespace)
          (or ,@convention-comments-keywords)
          (*? whitespace)
          (literal "(")
          (group (*? anychar))
          (literal ")")
          (literal ":"))))
        (decoration (rx (+? (group (+ not-newline)) (*? (literal ","))))))
    `((,plain
       (1 'font-lock-keyword-face t)
       (2 'bold t)
       (3 'italic append))
      (,decorated
       (1 'bold t)
       (2 'bold t)
       (3 'bold t)
       (4 'italic append))
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
               (colors convention-comments-decoration-colors)
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

(defsubst convention-comments-set-syntax (comment-string)
  "Add conventional comments syntax in ELisp.
Argument COMMENT-STRING represents one or more characters beginning a comment."
  (convention-comments-syntax-defaults)
  (font-lock-add-keywords nil (convention-comments-syntax-keywords
                               comment-string))
  (font-lock-update))

(defsubst convention-comments-unset-syntax (comment-string)
  "Remove conventional comments syntax in ELisp.
Argument COMMENT-STRING represents one or more characters beginning a comment."
  (convention-comments-syntax-defaults)
  (font-lock-remove-keywords nil (convention-comments-syntax-keywords
                                  comment-string))
  (font-lock-update))

(defun convention-comments--ask-type ()
  "Ask user for comment type to insert."
  (interactive)
  (let* ((start ?0) (prompt "Available choices:\n\n")
         choices choices-keys chosen)
    (dotimes (idx (length convention-comments-keywords))
      (let ((num (+ start idx))
            (val (nth idx convention-comments-keywords)))
        (push num choices-keys)
        (setf (alist-get (intern (number-to-string num)) choices) val)
        (setq prompt (format "%s%c = %s\n" prompt num val))))
    (setq prompt (format "%s\n\nC-g = Quit" prompt))
    (setq chosen (read-char-choice prompt (reverse choices-keys)))
    (let ((found (alist-get (intern (number-to-string chosen)) choices)))
      (when found
        (when convention-comments-insert-asked-type-as-comment
          (if comment-start
              (insert (format "%s " comment-start))
            (insert "# "))
        (insert (format "%s: " found)))))))

(defun convention-comments-syntax--activate ()
  "Add conventional comments syntax."
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    (define-key map (key-parse "C-;") #'convention-comments--ask-type))
  (if (and (boundp 'comment-start)
           comment-start
           (not (string= "" comment-start)))
      (convention-comments-set-syntax comment-start)
    (warn "convention: missing comment-start, fallback")
    (cond ((derived-mode-p 'emacs-lisp-mode)
           (convention-comments-set-syntax ";")))))

(defun convention-comments-syntax--deactivate ()
  "Remove conventional comments syntax."
  (if (and (boundp 'comment-start)
           comment-start
           (not (string= "" comment-start)))
      (convention-comments-unset-syntax comment-start)
    (warn "convention: missing comment-start, fallback")
    (cond ((derived-mode-p 'emacs-lisp-mode)
           (convention-comments-unset-syntax ";")))))


(provide 'convention-comments)
;;; convention-comments.el ends here
