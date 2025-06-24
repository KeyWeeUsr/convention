;;; conventional.el --- Enable conventional syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, conventional, mode, helper, git, comment, commit
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/KeyWeeUsr/conventional

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A collection of minor modes supporting conventional syntax.
;;
;; More on the initiatives on these sites:
;; * https://conventionalcommits.org
;; * https://conventionalcomments.org
;;
;; To enable it for comments everywhere, and for commits with basic
;; EDITOR=emacsclient, you can use:
;;
;; (add-hook
;;  'find-file-hook
;;  (lambda (&rest _)
;;    (conventional-comments-mode)
;;    (when (string= (file-name-base buffer-file-name) "COMMIT_EDITMSG")
;;      (conventional-commits-mode))))
;;
;; The hook worked for me both with plain term and with magit (with EDITOR env)

;;; Code:

(require 'conventional-comments)
(require 'conventional-commits)

;;;###autoload
(define-minor-mode conventional-comments-mode
  "Minor mode for conventional comments."
  :group 'conventional
  :lighter " conventional"
  (if conventional-comments-mode
      (conventional-comments-syntax--activate)
    (conventional-comments-syntax--deactivate)))

;;;###autoload
(define-minor-mode conventional-commits-mode
  "Minor mode for conventional commits."
  :group 'conventional
  :lighter " conventional"
  (if conventional-commits-mode
      (conventional-commits-syntax--activate)
    (conventional-commits-syntax--deactivate)))


(provide 'conventional)
;;; conventional.el ends here
