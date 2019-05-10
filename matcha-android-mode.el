;;; matcha-android-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'android-mode nil t)

(define-transient-command matcha-android-mode
  "Android"
  [["Start"
    ("a" "Start App" android-start-app)
    ("d" "DDMS" android-start-ddms)
    ("e" "Start Emulator" android-start-emulator)
    ("l" "Logcat" android-logcat)]
   ["Build"
    ("C" android-build-clean "Clean")
    ("t" android-build-test "Test")
    ("c" android-build-debug "Debug")]
   ["Install"
    ("u" android-build-install "Install")
    ("r" android-build-reinstall "Reinstall")
    ("i" android-build-uninstall "Uninstall")]])

(defun matcha-android-mode-set-launcher ()
  "Set up `android-mode' with `transient'."
  (matcha-set-mode-command
   :mode 'android-mode :command 'matcha-android-mode :minor-p t))

(provide 'matcha-android-mode)
;;; matcha-android-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
