;;; matcha-motion-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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

;; TODO: This could be one function or macro...

(defun +rake-to-device ()
  "Executes rake device."
  (interactive)
  (motion-execute-rake-command "device"))

(defun +rake-pod-install ()
  "Executes rake install."
  (interactive)
  (motion-execute-rake-command "pod:install"))

(defun +rake-pod-update ()
  "Executes rake update."
  (interactive)
  (motion-execute-rake-command "pod:update"))

(defun +rake-clean ()
  "Executes rake update."
  (interactive)
  (motion-execute-rake-command "clean"))

(defun +rake-spec ()
  "Executes rake spec."
  (interactive)
  (motion-execute-rake-command "spec"))

(defun +rake-run-sim ()
  "Tries to reload motion app.
If failure, run rake instead."
  (interactive)
  (unless (ignore-errors (motion-reload-app))
    (motion-execute-rake)))

(defun +rake-execute (command)
  "Enter in rake command to execute."
  (interactive "sEnter Rake command:")
  (motion-execute-rake-command command))

(defhydra matcha-motion-mode (:color blue)
  "Motion"
  ("pi" +rake-pod-install "Pod Install")
  ("pu" +rake-pod-update "Pod Update")
  ("c" +rake-clean "Rake Clean")
  ("d" +rake-to-device "Rake to Device")
  ("R" +rake-execute "Rake Execute")
  ("u" +rake-run-sim "Rake Run Sim")
  ("t" +rake-spec "Rake Spec")
  ("r" motion-execute-rake "Execute Rake"))

(defun matcha-motion-mode-set-launcher ()
  "Set up `hydra' launcher for `motion-mode'."
  (+add-mode-command #'matcha-motion-mode/body '(motion-mode)))

(provide 'matcha-motion-mode)
;;; matcha-motion-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
