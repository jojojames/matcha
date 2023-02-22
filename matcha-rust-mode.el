;;; matcha-rust-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
(require 'rust-mode nil t)
(require 'cargo nil t)

(transient-define-prefix matcha-rust-mode-cargo-test ()
  "Cargo Test"
  [["Test"
    ("t" "Test" cargo-process-test)
    ("c" "Current Test" cargo-process-current-test)
    ("f" "Current File" cargo-process-current-file-tests)]])

(transient-define-prefix matcha-rust-mode-cargo ()
  "Cargo"
  [
   :description (lambda () (format "Cargo: %s" (matcha-projectile-root)))
   ["Actions"
    ("x" "Run" cargo-process-run)
    ("X" "Run Example" cargo-process-run-example)
    ("v" "Check" cargo-process-check)
    ("l" "Clippy" cargo-process-clippy)
    ("=" "Format" rust-format-buffer)
    ("e" "Eval..." matcha-rust-mode-eval)
    ("t" "Test..." matcha-rust-mode-cargo-test)]
   ["Manage"
    ("i" "Init" cargo-process-init)
    ("n" "New" cargo-process-new)
    ("b" "Build" cargo-process-build)
    ("c" "Clean" cargo-process-clean)
    ("u" "Update" cargo-process-update)]
   ["Doc"
    ("d" "Doc" cargo-process-doc)
    ("D" "Doc & Open" cargo-process-doc-open)]
   ["Misc"
    ("." "Repeat Cargo Process" cargo-process-repeat)
    ("B" "Cargo Bench" cargo-process-bench)
    ("f" "Cargo Format" cargo-process-fmt)
    ("s" "Cargo Search" cargo-process-search)
    ("P" "Promote Module into Directory" rust-promote-module-into-dir)]])

(transient-define-prefix matcha-rust-mode-eval ()
  "Eval"
  [["Eval"
    ("r" "Region" rust-playpen-region)
    ("b" "Buffer" rust-playpen-buffer)]])

(defun matcha-rust-mode-set-launcher ()
  "Set up `rust-mode' with `transient'."
  (matcha-set-test-command
   :mode 'rust-mode :command 'matcha-rust-mode-cargo-test)
  (matcha-set-mode-command
   :mode 'rust-mode :command 'matcha-rust-mode-cargo)
  (matcha-set-eval-command
   :mode 'rust-mode :command 'matcha-rust-eval)
  (matcha-set-format-command
   :mode 'rust-mode :command 'rust-format-buffer))

(provide 'matcha-rust-mode)
;;; matcha-rust-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
