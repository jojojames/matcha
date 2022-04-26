;;; matcha-vc-hgcmd.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: transient, emacs, vc-hgcmd
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
(require 'matcha-hg-histedit nil t)
(require 'vc-hgcmd nil t)

(defun matcha-vc-hgcmd-shelve (name)
  "Create shelve named NAME."
  (interactive "sShelve name: ")
  (let ((command
         (concat "shelve"
                 (when (member "--unknown"
                               (transient-args 'matcha-vc-hgcmd-stash))
                   " --unknown")
                 (if (string-equal name "")
                     (user-error "Shelve name should not be empty")
                   (concat " -n " name)))))
    (message (format "Running: hg %s..." command))
    (vc-hgcmd-runcommand command)))

(defun matcha-vc-hgcmd-unshelve (name)
  "Restore a shelve named NAME."
  (interactive
   (list (completing-read "Shelve: " (vc-hgcmd-shelve-list))))
  (let ((command
         (concat "unshelve"
                 (when (member "--keep" (transient-args 'matcha-vc-hgcmd-stash))
                   " --keep")
                 (if (string-equal name "")
                     (user-error "Shelve name should not be empty")
                   (concat " " name)))))
    (message (format "Running: hg %s..." command))
    (vc-hgcmd-runcommand command)))

(defun matcha-vc-hgcmd-delete (name)
  "Delete a shelve named NAMED."
  (interactive
   (list (completing-read "Shelve: " (vc-hgcmd-shelve-list))))
  (let ((command
         (concat "shelve --delete"
                 (if (string-equal name "")
                     (user-error "Shelve name should not be empty")
                   (concat " " name)))))
    (message (format "Running: hg %s..." command))
    (vc-hgcmd-runcommand command)))

(define-transient-command matcha-vc-hgcmd-stash ()
  "Stash uncommited changes."
  ["Arguments"
   ("-u" "Store unknown files in the shelve" ("-u" "--unknown"))
   ("-k" "Keep shelve after unshelving" ("-k" "--keep"))]
  [["Shelve"
    ("z" "Shelve" matcha-vc-hgcmd-shelve)
    ("r" "Restore" matcha-vc-hgcmd-unshelve)
    ("x" "Delete" matcha-vc-hgcmd-delete)]])

(defvar matcha-vc-hgcmd-stash-evil-stash
  '(menu-item "" nil :filter (lambda (&optional _)
                               (when (eq vc-dir-backend 'Hgcmd)
                                 #'matcha-vc-hgcmd-stash))))

(defvar matcha-vc-hgcmd-histedit
  '(menu-item "" nil :filter (lambda (&optional _)
                               (when (or (eq vc-dir-backend 'Hgcmd)
                                         (eq major-mode 'vc-hgcmd-log-view-mode))
                                 #'matcha-hg-histedit))))

(defun matcha-vc-hgcmd-set-launcher ()
  "Set up `transient' launcher for `vc-git'."
  (when matcha-use-evil-p
    (with-eval-after-load 'evil
      (with-eval-after-load 'hg-histedit
        (evil-define-key* 'normal vc-hgcmd-log-view-mode-map
          "r" matcha-vc-hgcmd-histedit)
        (evil-define-key* 'normal vc-dir-mode-map
          "r" matcha-vc-hgcmd-histedit))
      (evil-define-key* 'normal vc-dir-mode-map
        "z" matcha-vc-hgcmd-stash-evil-stash)))
  (matcha-set-mode-command :mode 'vc-dir-mode :command #'matcha-vc-dir))

(provide 'matcha-vc-hgcmd)
;;; matcha-vc-hgcmd.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
