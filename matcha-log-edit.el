;;; matcha-log-edit.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://log-edithub.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: transient, emacs, log-edit
;; HomePage: https://github.com/jojojames/matcha

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
;;; Integration with Transient.

;;; Code:
(require 'matcha-base)
(require 'log-edit nil t)

(transient-define-prefix matcha-log-edit ()
  "Log Edit"
  [["Actions"
    ("c" "Done" log-edit-done)
    ("i" "Insert Changelog" log-edit-insert-changelog)
    ("d" "Show Diff" log-edit-show-diff)
    ("f" "Show Files" log-edit-show-files)
    ("k" "Kill Buffer" log-edit-kill-buffer)]
   ["Navigation"
    ("a" "Beginning of Line" log-edit-beginning-of-line)
    ("n" "Next Comment" log-edit-next-comment)
    ("p" "Previous Comment" log-edit-previous-comment)
    ("r" "Search Comment Backwards" log-edit-comment-search-backward)
    ("s" "Search Comment Forwards" log-edit-comment-search-forward)]
   ["Misc"
    ("?" "Help" log-edit-mode-help)]])

(defun matcha-log-edit-set-launcher ()
  "Set up `transient' launcher for `log-edit'."
  (matcha-set-mode-command :mode 'log-edit-mode :command #'matcha-log-edit))

(provide 'matcha-log-edit)
;;; matcha-log-edit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
