;;; matcha-rust-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
(require 'rust-mode nil t)
(require 'cargo nil t)

(defhydra matcha-rust-mode-cargo-test (:color blue :hint nil)
  "

    Rust Cargo Test
  ------------------------------------------------------------------------------
    _t_ Test    _c_ Current Test    _f_ Current File

"
  ("f" cargo-process-current-file-tests)
  ("c" cargo-process-current-test)
  ("t" cargo-process-test))

(defhydra matcha-rust-mode-cargo (:color blue :hint nil)
  "

    Cargo: %s(matcha-projectile-root)

    Manage         Run             ^^Doc            ^^Misc
  ------------------------------------------------------------------------------
    _i_ Init        _x_ Run          _d_ Doc           _._ Repeat Command
    _n_ New         _X_ Example      _D_ Doc & Open    _t_ Test
    _b_ Build       _v_ Check                        ^^_B_ Bench
    _c_ Clean       _l_ Clippy                       ^^_s_ Search
    _u_ Update                                     ^^^^_f_ Fmt

    Rust
  ------------------------------------------------------------------------------
    _e_ Eval    _P_ Promote Module    _=_ Format

"
  ;; Manage
  ("i" cargo-process-init)
  ("n" cargo-process-new)
  ("b" cargo-process-build)
  ("c" cargo-process-clean)
  ("u" cargo-process-update)

  ;; Run
  ("x" cargo-process-run)
  ("X" cargo-process-run-example)
  ("v" cargo-process-check)
  ("l" cargo-process-clippy)

  ;; Doc
  ("d" cargo-process-doc)
  ("D" cargo-process-doc-open)

  ;; Misc
  ("." cargo-process-repeat)
  ("t" matcha-rust-mode-cargo-test/body)
  ("B" cargo-process-bench)
  ("f" cargo-process-fmt)
  ("s" cargo-process-search)

  ;; Rust
  ("e" matcha-rust-eval/body)
  ("=" rust-format-buffer)
  ("P" rust-promote-module-into-dir))

(defhydra matcha-rust-eval (:color blue :hint nil)
  "

    Rust Evaluation
  ------------------------------------------------------------------------------
    _r_ Region    _b_ Buffer

"
  ("r" rust-playpen-region)
  ("b" rust-playpen-buffer))

(defun matcha-rust-mode-set-launcher ()
  "Set up `rust-mode' with `hydra'."
  (matcha-set-test-command
   :mode 'rust-mode :command 'matcha-rust-mode-cargo-test/body)
  (matcha-set-mode-command
   :mode 'rust-mode :command 'matcha-rust-mode-cargo/body)
  (matcha-set-eval-command
   :mode 'rust-mode :command 'matcha-rust-eval/body)
  (matcha-set-format-command
   :mode 'rust-mode :command 'rust-format-buffer))

(provide 'matcha-rust-mode)
;;; matcha-rust-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
