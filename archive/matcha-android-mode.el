;;; matcha-android-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'android-mode nil t)

(defhydra matcha-android-mode (:color blue)
  "Android"
  ("a" android-start-app "Start App")
  ("d" android-start-ddms "DDMS")
  ("e" android-start-emulator "Start Emulator")
  ("l" android-logcat "Logcat")
  ("C" android-build-clean "Clean")
  ("t" android-build-test "Test")
  ("c" android-build-debug "Debug")
  ("u" android-build-install "Install")
  ("r" android-build-reinstall "Reinstall")
  ("i" android-build-uninstall "Uninstall"))

(defun matcha-android-mode-set-launcher ()
  "Set up `android-mode' with `hydra'."
  (matcha-set-mode-command
   :mode 'android-mode :command 'matcha-android-mode/body :minor-p t))

(provide 'matcha-android-mode)
;;; matcha-android-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
