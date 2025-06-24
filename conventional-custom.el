;;; conventional-custom.el --- Comments syntax -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defgroup conventional nil
  "Basic configuration."
  :group 'convenience)

(defgroup conventional-comments nil
  "Basic configuration for comments."
  :group 'conventional)

(defgroup conventional-commits nil
  "Basic configuration for commits."
  :group 'conventional)

(defcustom conventional-comments-keywords
  '("praise" "nitpick" "suggestion"
    "issue" "todo" "question"
    "thought" "chore" "note")
  "A standard set of keywords."
  :type '(repeat string)
  :group 'conventional-comments)

(defcustom conventional-commits-keywords
  '("build" "chore" "ci"
    "docs" "feat" "fix"
    "perf" "refactor" "revert"
    "style" "test")
  "A standard set of keywords."
  :type '(repeat string)
  :group 'conventional-commits)

(defcustom conventional-comments-decoration-colors
  '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#17becf"
    "#bcbd22" "#7f7f7f" "#e377c2" "#ff6f61" "#30cfcf" "#b5bd00"
    "#a01920" "#4db6ac" "#9c89b8" "#8c564b" "#8dd35f" "#6a5acd")
  "List of decoration colors to alternate through."
  :type '(repeat color)
  :group 'conventional-comments)

(defcustom conventional-comments-insert-asked-type-as-comment t
  "When inserting an asked for comment type, prefix it with `comment-start'."
  :type 'boolean
  :group 'conventional-comments)

(defcustom conventional-commits-decoration-colors
  '("#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd" "#17becf"
    "#bcbd22" "#7f7f7f" "#e377c2" "#ff6f61" "#30cfcf" "#b5bd00"
    "#a01920" "#4db6ac" "#9c89b8" "#8c564b" "#8dd35f" "#6a5acd")
  "List of decoration colors to alternate through."
  :type '(repeat color)
  :group 'conventional-commits)

(defcustom conventional-commits-ask-for-type t
  "Ask for commit type when default COMMIT_EDITMSG is encountered."
  :type 'boolean
  :group 'conventional-commits)


(provide 'conventional-custom)
;;; conventional-custom.el ends here
