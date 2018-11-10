;;; matcha-magit.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs, magit
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
;; (require 'magit)

(defun matcha-magit-dir ()
  "Return `magit-toplevel' if `magit' is loaded, otherwise."
  (if (featurep 'magit)
      (magit-toplevel)
    "Not yet loaded."))

(defun matcha-magit-status-pick-repository ()
  "Calls `magit-status' with a prefix argument to allow picking the repository."
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively 'magit-status)))

(defun matcha-magit-status-or-switch-buffer ()
  "Switch to existing `magit-status' without updating or run `magit-status'.
If `magit' is not yet loaded yet, just call `magit-status' directly."
  (interactive)
  (if (not (featurep 'magit))
      (magit-status-internal default-directory)
    (if-let (magit-buffer
             (magit-mode-get-buffer
              #'magit-status-mode nil nil
              (magit-buffer-lock-value #'magit-status-mode nil)))
        (switch-to-buffer magit-buffer)
      (magit-status-internal default-directory))))

(defhydra matcha-magit-log (:color blue :hint nil)
  "

   Magit Log: %s(matcha-magit-dir)

    File                       Branch                  Reflog
  ------------------------------------------------------------------------------
    _f_ Current File         _p_ Pick Branch            _P_ Pick Branch
    _F_ Current File Popup   _c_ Current Branch         _C_ Current Branch
    _u_ Unmerged Commits     _h_ Head Branch            _H_ Head Branch
                           ^^_o_ Local + Head
                           ^^_a_ Local + Remote + Head
                           ^^_A_ Everything

"
  ("f" magit-log-buffer-file)
  ("F" magit-log-buffer-file-popup)
  ("u" magit-cherry)
  ("p" magit-log-other)
  ("c" magit-log-current)
  ("h" magit-log-head)
  ("o" magit-log-branches)
  ("a" magit-log-all-branches)
  ("A" magit-log-all)
  ("P" magit-reflog)
  ("C" magit-reflog-current)
  ("H" magit-reflog-head))

(defhydra matcha-magit (:color blue :hint nil)
  "

   Magit: %s(matcha-magit-dir)

    Status                    Blame                 ?
  ------------------------------------------------------------------------------
    _s_ Status               _b_ Blame               _p_ File Popup
    _g_ Switch to Status     _j_ Blob Next           _d_ Dispatch
    _L_ List Repos           _k_ Blob Previous       _l_ Log
    _r_ Pick Repo

    Search                           Diff            Repository
  ------------------------------------------------------------------------------
    _f_ Find File                _ef_ Ediff Files        ^^_c_ Clone
    _F_ Find File Other Window   _eb_ Ediff Buffers
                               ^^_ed_ Ediff Directories

"
  ("p" magit-file-popup)
  ("d" magit-dispatch-popup)
  ("f" magit-find-file)
  ("F" magit-find-file-other-window)
  ("j" magit-blob-next)
  ("k" magit-blob-previous)
  ("b" magit-blame-addition)
  ("l" matcha-magit-log/body)
  ("L" magit-list-repositories)
  ("r" matcha-magit-status-pick-repository)
  ("g" matcha-magit-status-or-switch-buffer)
  ("s" magit-status)
  ("c" magit-clone)
  ("ef" ediff-files)
  ("eF" ediff-files3)
  ("eb" ediff-buffers)
  ("eB" ediff-buffers3)
  ("ed" ediff-directories)
  ("eD" ediff-directories3))

(provide 'matcha-magit)
;;; matcha-magit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
