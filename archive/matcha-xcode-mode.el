;;; matcha-xcode-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
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
;; (require 'xcode-mode)

(defhydra matcha-xcode-mode (:color teal :hint nil)
  "
   Xcode: %(xcode-project-directory)

   Build                     Run                     Open
------------------------------------------------------------------------------------
  _ba_ Archive             _pi_ Run Pod Install   _op_ Open Project
  _bb_ Build               _rr_ Run               _os_ Open Storyboard
  _br_ Build and Run       _rt_ Run Tests         _ow_ Open Workspace
  _bt_ Builds Tests        _tt_ Run Test
  _bT_ Builds Tests Only
  _cc_ Clean

  "
  ("ba" xcode-xctool-archive)
  ("bb" xcode-xctool-build)
  ("br" xcode-xctool-build-and-run)
  ("bt" xcode-xctool-build-tests)
  ("bT" xcode-xctool-build-tests-only)
  ("cc" xcode-xctool-clean)
  ("dd" xcode-delete-derived-data "Delete Derived Data")
  ("op" xcode-open-storyboard)
  ("ow" xcode-open-workspace)
  ("os" xcode-open-project)
  ("pi" xcode-pod-install)
  ("rr" xcode-xctool-run)
  ("rt" xcode-xctool-run-tests)
  ("tt" xcode-xctool-test)
  ("q" nil "Cancel"))

(defun matcha-xcode-mode-set-launcher ()
  "Set `hydra' launcher for `xcode-mode'."
  (matcha-set-mode-command
   :mode 'xcode-mode :command #'matcha-xcode-mode/body :minor-p t))

(provide 'matcha-xcode-mode)
;;; matcha-xcode-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
