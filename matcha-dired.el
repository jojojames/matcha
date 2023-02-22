;;; matcha-dired.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'dired)

(defun matcha-dired-find-file ()
  "Like `find-file' but with `default-directory' set to the
one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively #'find-file)))

(transient-define-prefix matcha-dired-mode ()
  "Dired"
  [["Actions"
    ("f" "Find File" matcha-dired-find-file)
    ("d" "Delete File" dired-do-delete)
    ("r" "Rename File" dired-do-rename)
    ("y" "Copy File" dired-do-copy)
    ("+" "Create Directory" dired-create-directory)]
   ["Menu"
    ("m" "Mark..." matcha-dired-mark)
    ("o" "Operate..." matcha-dired-operate)
    ("%" "Regexp..." matcha-dired-regexp)
    ("i" "Immediate..." matcha-dired-immediate)]
   ["Misc"
    ("q" "Wdired" wdired-change-to-wdired-mode)
    ("Q" "Exit Wdired" wdired-finish-edit)
    ("R" "Query Replace" dired-do-query-replace-regexp)]])

(transient-define-prefix matcha-dired-operate ()
  "Operate"
  [["Actions"
    ("d" "Delete" dired-do-delete)
    ("r" "Rename" dired-do-rename)
    ("y" "Copy" dired-do-copy)
    ("@" "Shell Command" dired-do-shell-command)
    ("!" "Async Shell Command" dired-do-async-shell-command)]
   ["Ownership"
    ("cw" "Chown" dired-do-chown)
    ("cm" "Chmod" dired-do-chmod)
    ("cg" "Chgrp" dired-do-chgrp)
    ("t" "Touch" dired-do-touch)]
   ["Encryption"
    ("zz" "Compress" dired-do-compress)
    ("ze" "Encrypt" epa-dired-do-encrypt)
    ("zd" "Decrypt" epa-dired-do-decrypt)
    ("zv" "Verify" epa-dired-do-verify)
    ("zs" "Sign" epa-dired-do-sign)]]
  [["Search"
    ("f" "Search for Files" dired-do-search)
    ("sr" "ISearch Regexp" dired-do-isearch-regexp)
    ("ss" "ISearch" dired-do-isearch)
    ("q" "Query Replace" dired-do-query-replace-regexp)]
   ["Images"
    ("id" "Delete Tag" image-dired-delete-tag)
    ("it" "Tag Files" image-dired-tag-files)
    ("ic" "Comment Files" image-dired-dired-comment-files)
    ("is" "Display Thumbnails" image-dired-display-thumbs)]
   ["Misc"
    ("el" "Load Elisp" dired-do-load)
    ("eb" "Byte Compile Directories" dired-do-byte-compile)
    ("p" "Print" dired-do-print)
    ("h" "Hardlink" dired-do-hardlink)
    ("l" "Symlink" dired-do-symlink)]])

(transient-define-prefix matcha-dired-mark ()
  "Mark"
  [["Mark"
    ("m" "Mark" dired-mark)
    ("u" "Unmark" dired-unmark)
    ("C" "Change Marks" dired-change-marks)
    ("T" "Toggle Marks" dired-toggle-marks)]
   ["Mark All"
    ("U" "Unmark All" dired-unmark-all-marks)
    ("S" "Symlinks" dired-mark-symlinks)
    ("D" "Directories" dired-mark-directories)
    ("X" "Executables" dired-mark-executables)]]
  [["Flag for Deletion"
    ("d" "Mark for Deletion" dired-flag-file-deletion)
    ("g" "Garbage" dired-flag-garbage-files)
    ("b" "Backups" dired-flag-backup-files)
    ("s" "Autosaves" dired-flag-auto-save-files)
    ("c" "Clean Directory" dired-clean-directory)]
   ["Navigation"
    ("[" "Previous" dired-prev-marked-file)
    ("]" "Next" dired-next-marked-file)]])

(transient-define-prefix matcha-dired-regexp ()
  "Regexp"
  [["Mark"
    ("*" "Mark for Regexp" dired-mark-files-regexp)
    ("g" "Mark files containings Regexp" dired-mark-files-containing-regexp)
    ("t" "Image Tags for Regexp" image-dired-mark-tagged-files)]
   ["Apply"
    ("r" "Rename" dired-do-rename-regexp)
    ("c" "Copy" dired-do-copy-regexp)
    ("d" "Flag for Deletion" dired-flag-files-regexp)
    ("u" "Upcase" dired-upcase)
    ("l" "Downcase" dired-downcase)]
   ["Link"
    ("h" "Hardlink" dired-do-hardlink-regexp)
    ("s" "Symlink" dired-do-symlink-regexp)]])

(transient-define-prefix matcha-dired-immediate ()
  "Immediate"
  [["Find"
    ("ff" "Find" find-file)
    ("fv" "View" dired-view-file)
    ("fd" "Display" dired-display-file)
    ("fo" "Find in Other Window" dired-find-file-other-window)
    ("fp" "Find at Point" dired-find-file)]
   ["Misc"
    ("hd" "Hide Details in Buffer" dired-hide-details-mode)
    ("d" "Create Directory" dired-create-directory)
    ("r" "Refresh Buffer" revert-buffer)
    ("ww" "Wdired" wdired-change-to-wdired-mode)
    ("wq" "Finish Wdired" wdired-finish-edit)]]
  [["Compare"
    ("cd" "Directories" dired-compare-directories)
    ("cb" "With Backup" dired-backup-diff)
    ("cf" "With another File" dired-diff)]
   ["Search"
    ("ss" "ISearch" dired-isearch-filenames)
    ("sr" "ISearch Regexp" dired-isearch-filenames-regexp)]
   ["Images"
    ("ii" "Display" image-dired-dired-display-image)
    ("ie" "Display Externally" image-dired-dired-display-external)
    ("it" "Toggle Thumbnails" image-dired-dired-toggle-marked-thumbs)]])

(defun matcha-dired-set-launcher ()
  "Set up `dired' with `transient'."
  (matcha-set-mode-command :mode 'dired-sidebar-mode :command #'matcha-dired-mode)
  (matcha-set-mode-command :mode 'dired-mode :command #'matcha-dired-mode))

(provide 'matcha-dired)
;;; matcha-dired.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
