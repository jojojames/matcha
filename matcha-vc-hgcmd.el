;;; matcha-vc-hgcmd.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs, vc-hgcmd
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
;;; Integration with Hydra.

;;; Code:
(require 'matcha-base)
(require 'vc-hgcmd nil t)

(defun matcha-vc-hgcmd-shelve (name)
  "Create shelve named NAME."
  (interactive "sShelve name: ")
  (let ((commands '("shelve")))
    (when (member "--unknown" (transient-args))
      (push "--unknown" commands))
    (when (not (string-equal name ""))
      (push "-n" commands)
      (push name commands))
    (setq commands (nreverse commands)))
  (apply 'vc-hgcmd-command commands)
  (vc-dir-refresh))

(defun matcha-vc-hgcmd-unshelve (name)
  "Restore a shelve named NAME."
  (interactive "sShelve name: ")
  (let ((commands '("unshelve")))
    (when (member "--keep" (transient-args))
      (push "--keep" commands))
    (if (string-equal name "")
        (message "No shelve name...")
      (progn
        (push name commands)
        (setq commands (nreverse commands))
        (apply 'vc-hgcmd-command commands)
        (vc-dir-refresh)))))

(defun matcha-vc-hgcmd-delete (name)
  "Delete a shelve named NAMED."
  (interactive "sShelve name: ")
  (let ((commands '("--delete" "shelve")))
    (if (string-equal name "")
        (message "No shelve name...")
      (progn
        (push name commands)
        (setq commands (nreverse commands))
        (apply 'vc-hgcmd-command commands)
        (vc-dir-refresh)))))

(define-transient-command matcha-vc-hgcmd-stash ()
  "Stash uncommited changes."
  ["Arguments"
   ("-u" "Store unknown files in the shelve" ("-u" "--unknown"))
   ("-k" "Keep shelve after unshelving" ("-k" "--keep"))]
  [["Shelve"
    ("z" "Shelve" matcha-vc-hgcmd-shelve)
    ("r" "Restore" matcha-vc-hgcmd-unshelve)
    ("x" "Delete" matcha-vc-hgcmd-delete)]])

(provide 'matcha-vc-hgcmd)
;;; matcha-vc-hgcmd.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
