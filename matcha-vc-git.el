;;; matcha-vc-git.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs, vc-git
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
(require 'vc-git nil t)
(require 'vc-dir nil t)

(defvar matcha-vc-git-init-p nil "Variable tracking init state.")

(defun matcha-vc-git-log-edit-mode-p ()
  "Return t if current mode is `vc-git-log-edit-mode'."
  (eq major-mode 'vc-git-log-edit-mode))

(defun matcha-set-up-log-edit-transients ()
  "Set up `transient's for `matcha-log-edit'."
  (unless matcha-vc-git-init-p
    (setq matcha-vc-git-init-p t)
    (require 'matcha-log-edit)
    (transient-append-suffix 'matcha-log-edit "?"
      '("S" "Signoff" vc-git-log-edit-toggle-signoff :if matcha-vc-git-log-edit-mode-p))
    (transient-append-suffix 'matcha-log-edit "S"
      '("A" "Amend Commit" vc-git-log-edit-toggle-amend :if matcha-vc-git-log-edit-mode-p))))

(defun matcha-vc-git-set-launcher ()
  "Set up `transient' launcher for `vc-git'."
  (add-hook 'vc-dir-mode-hook 'matcha-set-up-log-edit-transients)
  (matcha-set-mode-command :mode 'vc-git-log-edit-mode :command #'matcha-log-edit)
  (matcha-set-mode-command :mode 'vc-dir-mode :command #'matcha-vc-dir))

(provide 'matcha-vc-git)
;;; matcha-vc-git.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
