;;; matcha-alchemist.el --- Integration with Hydra. -*- lexical-binding: t -*-

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

(defhydra matcha-alchemist-iex (:color blue :hint nil)
  "

    Alchemist IEX: %(matcha-projectile-root)

    Run                 Send               Loading
  ------------------------------------------------------------------------------
    _I_ Run            _l_ Line            _c_ Compile Buffer
    _i_ Run Project    _L_ Line and Go     _m_ Reload Module
                     ^^_r_ Region
                     ^^_R_ Region and Go

"

  ("I" alchemist-iex-run )
  ("i" alchemist-iex-project-run)
  ("l" alchemist-iex-send-current-line)
  ("L" alchemist-iex-send-current-line-and-go)
  ("r" alchemist-iex-send-region)
  ("R" alchemist-iex-send-region-and-go)
  ("c" alchemist-iex-compile-this-buffer)
  ("m" alchemist-iex-reload-module))

(defhydra matcha-alchemist-eval (:color blue :hint nil)
  "

    Alchemist Eval: %(matcha-projectile-root)

    Eval                Eval & Print               IEX
  ------------------------------------------------------------------------------
    _l_ Line           _L_ Line                _i_ IEX
    _r_ Region         _R_ Region
    _b_ Buffer         _B_ Buffer

    Eval (Quoted)       Eval & Print (Quoted)
  ------------------------------------------------------------------------------
    _j_ Line           _J_ Line
    _u_ Region         _U_ Region
    _v_ Buffer         _V_ Buffer

"
  ("i" matcha-alchemist-iex/body)
  ("l" alchemist-eval-current-line)
  ("L" alchemist-eval-print-current-line)
  ("r" alchemist-eval-region)
  ("R" alchemist-eval-print-region)
  ("b" alchemist-eval-buffer)
  ("B" alchemist-eval-print-buffer)
  ("j" alchemist-eval-quoted-current-line)
  ("J" alchemist-eval-print-quoted-current-line)
  ("u" alchemist-eval-quoted-region)
  ("U" alchemist-eval-print-quoted-region)
  ("v" alchemist-eval-quoted-buffer)
  ("V" alchemist-eval-print-quoted-buffer))

(defhydra matcha-alchemist-test (:color blue :hint nil)
  "

    Alchemist Test: %(matcha-projectile-root)

    Test                   Navigate               Rerun
  ------------------------------------------------------------------------------
    _t_ Test           _._ Next Test         _r_ Rerun Test
    _S_ Stale          _,_ Previous Test
    _b_ Buffer
    _p_ At Point
    _f_ File

"
  ("t" alchemist-mix-test)
  ("S" alchemist-mix-test-stale)
  ("b" alchemist-mix-test-this-buffer)
  ("p" alchemist-mix-test-at-point)
  ("f" alchemist-mix-test-file)
  ("." alchemist-test-jump-to-next-test)
  ("," alchemist-test-jump-to-previous-test)
  ("r" alchemist-mix-rerun-last-test))

(defhydra matcha-alchemist-mix (:color blue :hint nil)
  "

    Alchemist Mix
  ------------------------------------------------------------------------------
    _m_ Mix     _c_ Mix Compile    _r_ Mix Run    _h_ Mix Help

"
  ("m" alchemist-mix)
  ("c" alchemist-mix-compile)
  ("r" alchemist-mix-run)
  ("h" alchemist-mix-help))

(defhydra matcha-alchemist-help (:color blue :hint nil)
  "

    Alchemist Help: %(matcha-projectile-root)
  ------------------------------------------------------------------------------
    _:_ Help            _m_ Mix Help    _H_ History Help
    _h_ Help at Point
    _r_ Help for Region

"
  ("m" alchemist-mix-help)
  (":" alchemist-help)
  ("H" alchemist-help-history)
  ("h" alchemist-help-search-at-point)
  ("r" alchemist-help-search-marked-region))

(defhydra matcha-alchemist-phoenix (:color blue :hint nil)
  "

   Phoenix: %(matcha-projectile-root)

    ^Routes^             ^Model^              ^Resources^
  ------------------------------------------------------------------------------
    _r_ Routes       _c_ Controllers     _s_ Static
    _R_ Router       _l_ Channels        _t_ Template
                     ^^_m_ Models          _v_ View
                                         ^^^^_w_ Web

"
  ("R" alchemist-phoenix-routes)
  ("r" alchemist-phoenix-router)
  ("c" alchemist-phoenix-find-controllers)
  ("l" alchemist-phoenix-find-channels)
  ("m" alchemist-phoenix-find-models)
  ("s" alchemist-phoenix-find-static)
  ("t" alchemist-phoenix-find-templates)
  ("v" alchemist-phoenix-find-views)
  ("w" alchemist-phoenix-find-web))

(defhydra matcha-alchemist-mode (:color blue :hint nil)
  "

    Alchemist: %(matcha-projectile-root)

    ^Do^             ^Run^               ^Execute^        ^Compile^
  ------------------------------------------------------------------------------
    _p_ Phoenix   _r_ Phx Server   _xb_ Buffer     _cb_ Buffer
    _e_ Eval      _z_ IEX          _xf_ File       _cf_ File
    _t_ Test                       ^^_xx_ Execute    _cc_ Compile
    _i_ IEX
    _m_ Mix
    _h_ Help
    _l_ List Definitions

"
  ("p" matcha-alchemist-phoenix/body)
  ("e" matcha-alchemist-eval/body)
  ("t" matcha-alchemist-test/body)
  ("i" matcha-alchemist-iex/body)
  ("m" matcha-alchemist-mix/body)
  ("h" matcha-alchemist-help/body)
  ("r" matcha-alchemist-mix-phoenix-server)
  ("z" alchemist-iex-run)
  ("l" alchemist-goto-list-symbol-definitions)
  ("xb" alchemist-execute-this-buffer)
  ("xf" alchemist-execute-file)
  ("xx" alchemist-execute)
  ("cb" alchemist-compile-this-buffer)
  ("cf" alchemist-compile-file)
  ("cc" alchemist-compile))

(defun matcha-alchemist-set-launcher ()
  "Set up `elixir-mode' with `hydra'."
  (matcha-set-mode-command
   :mode 'alchemist-mode :command #'matcha-alchemist-mode/body :minor-p t)

  (matcha-set-eval-command
   :mode 'alchemist-mode :command #'matcha-alchemist-eval/body :minor-p t)

  (matcha-set-test-command
   :mode alchemist-mode :command #'matcha-alchemist-test/body :minor-p t))

(provide 'matcha-alchemist)
;;; matcha-elixir-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
