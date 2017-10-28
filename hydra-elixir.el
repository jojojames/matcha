;;; hydra-elixir.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/hydra-integrations
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/hydra-integrations

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
(require 'hydra-integration-base)
(require 'alchemist)

(defmacro +define-alchemist-function (&rest mix-commands)
  "Create a command that wraps `alchemist-mix-execute'.
`mix-command' should be a string that will be the mix command to be ran.

If `mix-command' is \"phoenix.server\", then the resulting `defun' will be:

`+alchemist-mix-phoenix-server'."
  `(progn
     ,@(cl-loop
        for command in mix-commands
        collect
        (let ((funsymbol
               (intern (concat "+alchemist-mix-"
                               (s-replace "." "-" command)))))
          `(defun ,funsymbol ()
             ,(concat "Run $ mix " command ".")
             (interactive)
             (when-let (process (get-buffer-process "*alchemist mix*"))
               (set-process-query-on-exit-flag process nil))
             (alchemist-mix-execute (list ,command)))))))

(+define-alchemist-function "phoenix.server" "deps.get")

(defhydra hydra-alchemist-iex (:color blue :columns 4)
  "IEX"
  ("c" alchemist-iex-compile-this-buffer "Compile Buffer")
  ("b" alchemist-iex-compile-this-buffer "Compile Buffer")
  ("I" alchemist-iex-run "Run")
  ("i" alchemist-iex-project-run "Project Run")
  ("l" alchemist-iex-send-current-line "Send Current Line")
  ("L" alchemist-iex-send-current-line-and-go "Send Current Line and Go")
  ("m" alchemist-iex-reload-module "Reload Module")
  ("r" alchemist-iex-send-region "Send Region")
  ("R" alchemist-iex-send-region-and-go "Send Region and Go"))

(defhydra hydra-alchemist-eval (:color blue :columns 4)
  "Eval"
  ("i" hydra-alchemist-iex/body "IEX")
  ("b" alchemist-eval-buffer "Buffer")
  ("B" alchemist-eval-print-buffer "Print Buffer")
  ("l" alchemist-eval-current-line "Current Line")
  ("L" alchemist-eval-print-current-line "Print Current Line")
  ("r" alchemist-eval-region "Region")
  ("R" alchemist-eval-print-region "Print Region")
  ("j" alchemist-eval-quoted-current-line "Quoted Current Line")
  ("J" alchemist-eval-print-quoted-current-line "Print Quoted Current Line")
  ("u" alchemist-eval-quoted-region "Quoted Region")
  ("U" alchemist-eval-print-quoted-region "Print Quoted Region")
  ("v" alchemist-eval-quoted-buffer "Quoted Buffer")
  ("V" alchemist-eval-print-quoted-buffer "Print Quoted Buffer"))

(defhydra hydra-alchemist-test (:color blue :columns 4)
  "Test"
  ("t" alchemist-mix-test "Mix Test")
  ("a" alchemist-mix-test "Mix Test")
  ("S" alchemist-mix-test-stale "Mix Test Stale")
  ("b" alchemist-mix-test-this-buffer "Mix Test Buffer")
  ("p" alchemist-mix-test-at-point "Mix Test at Point")
  ("f" alchemist-mix-test-file "Mix Test File")
  ("." alchemist-test-jump-to-next-test "Next Test")
  ("/" alchemist-test-jump-to-previous-test "Prev Test")
  ("N" alchemist-test-jump-to-next-test "Next Test")
  ("P" alchemist-test-jump-to-previous-test "Prev Test")
  ("r" alchemist-mix-rerun-last-test "Rerun Test"))

(defhydra hydra-alchemist-mix (:color blue :columns 4)
  "Mix"
  ("m" alchemist-mix "Mix")
  (";" alchemist-mix "Mix")
  (":" alchemist-mix "Mix")
  ("c" alchemist-mix-compile "Compile")
  ("x" alchemist-mix-run "Run")
  ("h" alchemist-mix-help "Help"))

(defhydra hydra-alchemist-help (:color blue)
  "Help"
  ("m" alchemist-mix-help "Mix")
  (":" alchemist-help "Help")
  ("H" alchemist-help-history "Help History")
  ("h" alchemist-help-search-at-point "Help at Point")
  ("r" alchemist-help-search-marked-region "Help Marked Region"))

(defhydra hydra-alchemist-phoenix (:color blue :columns 4)
  "Phoenix"
  ("R" alchemist-phoenix-routes "Routes")
  ("c" alchemist-phoenix-find-controllers "Controllers")
  ("l" alchemist-phoenix-find-channels "Channels")
  ("m" alchemist-phoenix-find-models "Models")
  ("r" alchemist-phoenix-router "Router")
  ("s" alchemist-phoenix-find-static "Static")
  ("t" alchemist-phoenix-find-templates "Template")
  ("v" alchemist-phoenix-find-views "View")
  ("w" alchemist-phoenix-find-web "Web"))

(defhydra hydra-alchemist-mode (:color blue :columns 4)
  "Alchemist"
  ("u" +alchemist-mix-phoenix-server "Run Phoenix Server")
  ("r" +alchemist-mix-phoenix-server "Run Phoenix Server")
  ("p" hydra-alchemist-phoenix/body "Phoenix")
  ("e" hydra-alchemist-eval/body "Eval")
  ("t" hydra-alchemist-test/body "Test")
  ("z" alchemist-iex-run "Run IEX")
  ("i" hydra-alchemist-iex/body "IEX")
  ("m" hydra-alchemist-mix/body "Mix")
  ("h" hydra-alchemist-help/body "Help")
  ("l" alchemist-goto-list-symbol-definitions "Goto Definitions")
  ("xb" alchemist-execute-this-buffer "Execute Buffer")
  ("xf" alchemist-execute-file "Execute File")
  ("xx" alchemist-execute "Execute")
  ("cb" alchemist-compile-this-buffer "Compile Buffer")
  ("cf" alchemist-compile-file "Compile File")
  ("c:" alchemist-compile "Compile"))

(+add-minor-mode-command #'hydra-alchemist-mode/body '(alchemist-mode))
(+add-minor-eval-command #'hydra-alchemist-eval/body '(alchemist-mode))
(+add-minor-test-command #'hydra-alchemist-test/body '(alchemist-mode))

(provide 'hydra-elixir)
;;; hydra-elixir.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
