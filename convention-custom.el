;;; convention-custom.el --- Comments syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defgroup convention nil
  "Basic configuration."
  :group 'convenience)

(defgroup convention-comments nil
  "Basic configuration for comments."
  :group 'convention)

(defgroup convention-commits nil
  "Basic configuration for commits."
  :group 'convention)

(defcustom convention-comments-keywords
  '("praise" "nitpick" "suggestion"
    "issue" "todo" "question"
    "thought" "chore" "note")
  "A standard set of keywords."
  :type '(repeat string)
  :group 'convention-comments)

(defcustom convention-commits-keywords
  '("build" "chore" "ci"
    "docs" "feat" "fix"
    "perf" "refactor" "revert"
    "style" "test")
  "A standard set of keywords."
  :type '(repeat string)
  :group 'convention-commits)

(defcustom convention-comments-decoration-colors
  '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#17becf"
    "#bcbd22" "#7f7f7f" "#e377c2" "#ff6f61" "#30cfcf" "#b5bd00"
    "#a01920" "#4db6ac" "#9c89b8" "#8c564b" "#8dd35f" "#6a5acd")
  "List of decoration colors to alternate through."
  :type '(repeat color)
  :group 'convention-comments)

(defcustom convention-commits-decoration-colors
  '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#17becf"
    "#bcbd22" "#7f7f7f" "#e377c2" "#ff6f61" "#30cfcf" "#b5bd00"
    "#a01920" "#4db6ac" "#9c89b8" "#8c564b" "#8dd35f" "#6a5acd")
  "List of decoration colors to alternate through."
  :type '(repeat color)
  :group 'convention-commits)


(provide 'convention-custom)
;;; convention-custom.el ends here
