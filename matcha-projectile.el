;; matcha-projectile.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: transient, emacs
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
;; (require 'projectile)

(defun matcha-projectile-root ()
  "Return path `matcha-projectile' can print in heading."
  (if (projectile-project-p)
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory (projectile-project-root))))
    "Not in Project"))

(define-transient-command matcha-projectile-cache ()
  "Cache"
  [["Cache"
    ("c" "Invalidate" projectile-invalidate-cache)
    ("d" "Remove Known Project" projectile-remove-known-project)
    ("k" "Cache Current File" projectile-cache-current-file)
    ("s" "Cleanup Known Projects" projectile-cleanup-known-projects)]])

(define-transient-command matcha-projectile ()
  "Projectile"
  [["Find"
    ("f" "File" projectile-find-file)
    ("F" "File Other Window" projectile-find-file-other-window)
    ("l" "File DWIM" projectile-find-file-dwim)
    ("L" "File DWIM Other Window" projectile-find-file-dwim-other-window)
    ("o" "Other File" projectile-find-other-file)
    ("O" "Other file Other Window" projectile-find-other-file-other-window)
    ("r" "Recent File" projectile-recentf)
    ("u" "Test File" projectile-find-test-file)]
   ["Buffers & Directories"
    ("b" "Buffer" projectile-switch-to-buffer)
    ("B" "Buffer Other Window" projectile-switch-to-buffer-other-window)
    ("K" "Kill Project Buffers" projectile-kill-buffers)
    ("d" "Directory" projectile-find-dir)
    ("D" "Directory Other Window" projectile-find-dir-other-window)]
   ["Actions"
    ("R" "Replace Regexp" projectile-replace-regexp)
    ("S" "Replace" projectile-replace)
    ("U" "Run Tests" projectile-test-project)
    ("m" "Compile Project" projectile-compile-project)
    ("c" "Run Async Shell Command" projectile-run-async-shell-command-in-root)
    ("C" "Run Command" projectile-run-command-in-root)]]
  [["Modes"
    ("g" "Version Control" projectile-vc)
    ("h" "Dired" projectile-dired)
    ("i" "IBuffer" projectile-ibuffer)]
   ["Search"
    ("a" "AG" projectile-ag)
    ("A" "Grep" projectile-grep)
    ("s" "Multi Occur" projectile-multi-occur)
    ("t" "Find Tag" projectile-find-tag)
    ("T" "Regenerate Tags" projectile-regenerate-tags)]
   ["Manage"
    ("p" "Switch Project" projectile-switch-project)
    ("I" "Project Info" projectile-project-info)
    ("k" "Cache..." matcha-projectile-cache)
    ("P" "Commander" projectile-commander)]])

(provide 'matcha-projectile)
;;; matcha-projectile.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
