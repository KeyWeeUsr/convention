;;; convention.el --- Enable conventional syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, conventional, mode, helper, git, comment, commit
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/KeyWeeUsr/convention

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

;;; Code:

(require 'convention-comments)
(require 'convention-commits)

;;;###autoload
(define-minor-mode convention-comments-mode
  :group 'convention
  :lighter " convention"
  (if convention-comments-mode
      (convention-comments-syntax--activate)
    (convention-comments-syntax--deactivate)))

;;;###autoload
(define-minor-mode convention-commits-mode
  :group 'convention
  :lighter " convention"
  (if convention-commits-mode
      (convention-commits-syntax--activate)
    (convention-commits-syntax--deactivate)))


(provide 'convention)
;;; convention.el ends here
