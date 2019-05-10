;;; matcha-cc-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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

;; https://stackoverflow.com/questions/19953924/how-do-you-run-java-codes-in-emacs
(defun java-eval-nofocus ()
  "Run current program (that requires no input)."
  (interactive)
  (let* ((source (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension source))
         (class (concat out ".class")))
    (save-buffer)
    (shell-command (format "rm -f %s && javac %s" class source))
    (if (file-exists-p class)
        (compile (format "java %s" out))
      (progn
        (set (make-local-variable 'compile-command)
             (format "javac %s" source))
        (command-execute 'compile)))))

(defhydra matcha-java-mode (:color blue)
  ("u" java-eval-nofocus "Eval"))

(defun matcha-cc-mode-clang-format-region-or-buffer ()
  "If clang-format is not available, do the default indenting.
Otherwise try to use clang-format. Indents region if there's a selection,
otherwise buffer is formatted."
  (interactive)
  (if (and (executable-find "clang-format")
           (locate-dominating-file default-directory ".clang-format"))
      (if (region-active-p)
          (call-interactively 'clang-format-region)
        (clang-format-buffer))
    (indent-region-or-buffer)))

(defun matcha-cc-mode-mode-set-launcher ()
  "Set up `hydra' launcher for `java-mode'."
  (matcha-set-mode-command :mode 'java-mode :command #'matcha-java-mode/body)

  (matcha-set-format-command
   :mode '(c-mode c++-mode objc-mode)
   :command #'matcha-cc-mode-clang-format-region-or-buffer))

(provide 'matcha-cc-mode)
;;; matcha-cc-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
