;;; convention-comments.el --- Comments syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defsubst convention-comments-syntax-defaults ()
  "Basic setup for font-lock."
  (font-lock-set-defaults)
  (setq-local font-lock-keywords-case-fold-search t))

(defvar convention-comments-decoration-colors
  '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#17becf"
    "#bcbd22" "#7f7f7f" "#e377c2" "#ff6f61" "#30cfcf" "#b5bd00"
    "#a01920" "#4db6ac" "#9c89b8" "#8c564b" "#8dd35f""#6a5acd")
  "List of decoration colors to alternate through.")

(defsubst convention-comments-syntax-elisp-keywords ()
  "Create conventional comments keywords for ELisp."
  `((,(rx (literal ";") (*? whitespace)
          (group (or "praise" "nitpick" "suggestion"
                     "issue" "todo" "question"
                     "thought" "chore" "note"))
          (*? not-newline)
          (group (literal ":"))
          (group (*? not-newline))
          line-end)
     (1 'font-lock-keyword-face t)
     (2 'bold t)
     (3 'italic append))
    (,(rx (literal ";") (*? whitespace)
          (or "praise" "nitpick" "suggestion"
              "issue" "todo" "question"
              "thought" "chore" "note")
          (*? not-newline)
          (group (literal "("))
          (*? not-newline)
          (group (literal ")"))
          (group (literal ":"))
          (group (*? not-newline))
          line-end)
     (1 'bold t)
     (2 'bold t)
     (3 'bold t)
     (4 'italic append))
    (,(rx (literal ";") (*? whitespace)
          (or "praise" "nitpick" "suggestion"
              "issue" "todo" "question"
              "thought" "chore" "note")
          (*? whitespace)
          (literal "(")
          (group (*? anychar))
          (literal ")")
          (literal ":"))
     (,(rx (+? (group (+ not-newline)) (*? (literal ","))))
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
      ))))

(defsubst convention-comments-set-syntax-elisp ()
  "Add conventional comments syntax in ELisp."
  (convention-comments-syntax-defaults)
  (font-lock-add-keywords nil (convention-comments-syntax-elisp-keywords))
  (font-lock-update))

(defsubst convention-comments-unset-syntax-elisp ()
  "Remove conventional comments syntax in ELisp."
  (convention-comments-syntax-defaults)
  (font-lock-remove-keywords nil (convention-comments-syntax-elisp-keywords))
  (font-lock-update))

(defun convention-comments-syntax--activate ()
  "Add conventional comments syntax."
  (cond ((derived-mode-p 'emacs-lisp-mode)
         (convention-comments-set-syntax-elisp))))

(defun convention-comments-syntax--deactivate ()
  "Remove conventional comments syntax."
  (cond ((derived-mode-p 'emacs-lisp-mode)
         (convention-comments-unset-syntax-elisp))))


(provide 'convention-comments)
;;; convention-comments.el ends here
