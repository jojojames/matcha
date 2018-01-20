;;; matcha-ruby-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'projectile-rails)
;; (require 'robe)

(defhydra matcha-bundler-mode (:color blue :columns 4)
  "Bundler"
  ("o" bundle-open "Open")
  ("c" bundle-console "Console")
  ("C" bundle-check "Check")
  ("i" bundle-install "Install")
  ("u" bundle-update "Update")
  ("e" bundle-exec "Exec")
  ("O" bundle-outdated "Outdated")
  ("s" bundle-show "Show")
  ("v" bundle-version "Version")
  ("b" bundle-command "Command"))

(defhydra matcha-ruby-mode (:color blue)
  "Ruby"
  ("B" matcha-bundler-mode/body "Bundler"))

(defhydra matcha-robe-mode (:color blue)
  "Robe"
  ("K" robe-rails-refresh "Rails Refresh")
  ("B" matcha-bundler-mode/body "Bundler"))

(defhydra matcha-rails-mode (:color blue :columns 4)
  "Rails"
  ("a" projectile-rails-find-locale "Locale")
  ("A" projectile-rails-find-job "Job")
  ("B" matcha-robe-mode/body)
  ("c" projectile-rails-find-controller "Controller")
  ("C" projectile-rails-find-current-controller "Current Controller")
  ("d" projectile-rails-dbconsole "DB Console")
  ("D" projectile-rails-console "Console")
  ("e" projectile-rails-find-environment "Environment")
  ("E" projectile-rails-generate "Generate")
  ("f" projectile-rails-find-feature "Feature")
  ("F" projectile-rails-find-validator "Validator")
  ("gg" projectile-rails-goto-gemfile "Gemfile")
  ("gr" projectile-rails-goto-routes "Routes")
  ("gd" projectile-rails-goto-schema "Schema")
  ("gs" projectile-rails-goto-seeds "Seeds")
  ("gh" projectile-rails-goto-spec-helper "Spec Helper")
  ("h" projectile-rails-find-helper "Helper")
  ("H" projectile-rails-find-current-helper "Current Helper")
  ("i" projectile-rails-find-initializer "Initializer")
  ("j" projectile-rails-find-javascript "Javascript")
  ("J" projectile-rails-find-stylesheet "Stylesheet")
  ("K" robe-rails-refresh "Rails Refresh")
  ("l" projectile-rails-find-lib "Lib")
  ("L" projectile-rails-find-layout "Layout")
  ("m" projectile-rails-find-model "Model")
  ("M" projectile-rails-find-current-model "Current Model")
  ("n" projectile-rails-find-migration "Migration")
  ("N" projectile-rails-find-current-migration "Current Migration")
  ("o" projectile-rails-find-log "Log")
  ("p" projectile-rails-find-spec "Spec")
  ("P" projectile-rails-find-current-spec "Current Spec")
  ("r" projectile-rails-rake "Rake")
  ("R" projectile-rails-find-rake-task "Rake Task")
  ("t" projectile-rails-find-test "Test")
  ("T" projectile-rails-find-current-test "Current")
  ("u" projectile-rails-server "Server")
  ("v" projectile-rails-find-view "View")
  ("V" projectile-rails-find-current-view "Current View")
  ("x" projectile-rails-extract-region "Extract Region")
  ("y" projectile-rails-find-fixture "Fixture")
  ("Y" projectile-rails-find-current-fixture "Current Fixture")
  ("@" projectile-rails-find-mailer "Mailer")
  ("<RET>" projectile-rails-goto-file-at-point "File At Point"))

(defun matcha-ruby-mode-set-launcher ()
  "Set up `hydra' launcher for `ruby-mode'."
  (matcha-set-mode-command
   :mode 'ruby-mode :command #'matcha-ruby-mode/body)
  (matcha-set-mode-command
   :mode 'robe-mode :command #'matcha-robe-mode/body :minor-p t)
  (matcha-set-mode-command
   :mode 'projectile-rails-mode :command #'matcha-rails-mode/body :minor-p t))

(provide 'matcha-ruby-mode)
;;; matcha-ruby-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
