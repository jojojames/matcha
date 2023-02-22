;;; matcha-alchemist.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'alchemist nil t)

(defmacro matcha-define-alchemist-function (&rest mix-commands)
  "Create a command that wraps `alchemist-mix-execute'.
`mix-command' should be a string that will be the mix command to be ran.

If `mix-command' is \"phoenix.server\", then the resulting `defun' will be:

`matcha-alchemist-mix-phoenix-server'."
  `(progn
     ,@(cl-loop
        for command in mix-commands
        collect
        (let ((funsymbol
               (intern (concat "matcha-alchemist-mix-"
                               (replace-regexp-in-string "\\." "-" command)))))
          `(defun ,funsymbol ()
             ,(concat "Run $ mix " command ".")
             (interactive)
             (when-let (process (get-buffer-process "*alchemist mix*"))
               (set-process-query-on-exit-flag process nil))
             (alchemist-mix-execute (list ,command)))))))

(matcha-define-alchemist-function "phoenix.server" "deps.get")

(transient-define-prefix matcha-alchemist-iex ()
  "IEX"
  [["Run"
    ("I" "Run" alchemist-iex-run )
    ("i" "Run Project" alchemist-iex-project-run)]
   ["Send"
    ("l" "Current Line" alchemist-iex-send-current-line)
    ("L" "Current Line & Go" alchemist-iex-send-current-line-and-go)
    ("r" "Region" alchemist-iex-send-region)
    ("R" "Region & Go" alchemist-iex-send-region-and-go)]
   ["Misc"
    ("c" "Compile Buffer" alchemist-iex-compile-this-buffer)
    ("m" "Reload Module" alchemist-iex-reload-module)]])

(transient-define-prefix matcha-alchemist-eval ()
  "Eval"
  [["Eval"
    ("l" "Line" alchemist-eval-current-line)
    ("r" "Region" alchemist-eval-region)
    ("b" "Buffer" alchemist-eval-buffer)]
   ["Eval & Print"
    ("L" "Line" alchemist-eval-print-current-line)
    ("R" "Region" alchemist-eval-print-region)
    ("B" "Buffer" alchemist-eval-print-buffer)]]
  [["Eval (Quoted)"
    ("j" "Line" alchemist-eval-quoted-current-line)
    ("u" "Region" alchemist-eval-quoted-region)
    ("v" "Buffer" alchemist-eval-quoted-buffer)]
   ["Eval & Print (Quoted)"
    ("J" "Line" alchemist-eval-print-quoted-current-line)
    ("U" "Region" alchemist-eval-print-quoted-region)
    ("V" "Buffer" alchemist-eval-print-quoted-buffer)]
   ["IEX"
    ("i" "IEX..." matcha-alchemist-iex)]])

(transient-define-prefix matcha-alchemist-test ()
  "IEX"
  [["Test"
    ("t" "Test" alchemist-mix-test)
    ("S" "Stale" alchemist-mix-test-stale)
    ("b" "Buffer" alchemist-mix-test-this-buffer)
    ("p" "At Point" alchemist-mix-test-at-point)
    ("f" "File" alchemist-mix-test-file)]
   ["Navigation"
    ("." "Next Test" alchemist-test-jump-to-next-test)
    ("," "Previous Test" alchemist-test-jump-to-previous-test)]
   ["Misc"
    ("r" "Rerun Test" alchemist-mix-rerun-last-test)]])

(transient-define-prefix matcha-alchemist-mix ()
  "Mix"
  [["Mix"
    ("m" "Mix" alchemist-mix)
    ("c" "Compile" alchemist-mix-compile)
    ("r" "Run" alchemist-mix-run)
    ("h" "Help" alchemist-mix-help)]])

(transient-define-prefix matcha-alchemist-help ()
  "Help"
  [["Help"
    ("?" "Help" alchemist-help)
    ("h" "At Point" alchemist-help-search-at-point)
    ("r" "Help for Region" alchemist-help-search-marked-region)]
   ["Misc"
    ("l" "List Symbol Definitions" alchemist-goto-list-symbol-definitions)
    ("m" "Mix Help" alchemist-mix-help)
    ("H" "History Help" alchemist-help-history)]])

(transient-define-prefix matcha-alchemist-phoenix ()
  "Phoenix"
  [["Routes"
    ("R" "Routes" alchemist-phoenix-routes)
    ("r" "Router" alchemist-phoenix-router)]
   ["Model"
    ("c" "Controllers" alchemist-phoenix-find-controllers)
    ("l" "Channels" alchemist-phoenix-find-channels)
    ("m" "Models" alchemist-phoenix-find-models)]
   ["Resources"
    ("s" "Static" alchemist-phoenix-find-static)
    ("t" "Templates" alchemist-phoenix-find-templates)
    ("v" "Views" alchemist-phoenix-find-views)
    ("w" "Web" alchemist-phoenix-find-web)]])

(transient-define-prefix matcha-alchemist-mode ()
  "Alchemist"
  [["Actions"
    ;; FIXME: Dynamically plug in the phoenix related `transients'.
    ("p" "Phoenix..." matcha-alchemist-phoenix)
    ("e" "Eval..." matcha-alchemist-eval)
    ("t" "Test..." matcha-alchemist-test)
    ("i" "Iex..." matcha-alchemist-iex)
    ("m" "Mix..." matcha-alchemist-mix)
    ("h" "Help..." matcha-alchemist-help)]
   ["Run"
    ("r" "Mix Phoenix Server" matcha-alchemist-mix-phoenix-server)
    ("z" "Run" alchemist-iex-run)]
   ["Execute"
    ("xx" "Execute" alchemist-execute)
    ("xb" "Buffer" alchemist-execute-this-buffer)
    ("xf" "File" alchemist-execute-file)]
   ["Compile"
    ("cc" "Compile" alchemist-compile)
    ("cb" "Buffer" alchemist-compile-this-buffer)
    ("cf" "File" alchemist-compile-file)]])

(defun matcha-alchemist-set-launcher ()
  "Set up `elixir-mode' with `transient'."
  (matcha-set-mode-command
   :mode 'alchemist-mode :command #'matcha-alchemist-mode :minor-p t)

  (matcha-set-eval-command
   :mode 'alchemist-mode :command #'matcha-alchemist-eval :minor-p t)

  (matcha-set-test-command
   :mode alchemist-mode :command #'matcha-alchemist-test :minor-p t))

(provide 'matcha-alchemist)
;;; matcha-elixir-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
