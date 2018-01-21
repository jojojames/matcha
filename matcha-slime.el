;;; matcha-slime.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'slime nil t)

;; TODO: Add Debug Hydra

(defhydra matcha-slime-eval (:color blue :hint nil)
  "

    Slime Eval: %s(buffer-name)

    Eval                    Print                        Misc
  ------------------------------------------------------------------------------
    _e_ Expression     _j_ Expression          _l_ Eval Expression in REPL
    _r_ Region         _R_ Region PPrint       _u_ Undefine Function
    _b_ Buffer         _J_ Expression PPrint   _n_ Remove Notes
    _d_ Defun          _I_ Eval and Inspect    _i_ Interrupt Lisp
    _v_ Defvar
    _i_ Interactively
    _E_ Edit Value

    Compile
  ------------------------------------------------------------------------------
    _cc_ Compile
    _cl_ Compile and Load
    _cd_ Compile Defun
    _cr_ Compile Region
    _L_ Load File

"
  ("cc" slime-compile-file)
  ("cl" slime-compile-and-load-file)
  ("cd" slime-compile-defun)
  ("cr" slime-compile-region)
  ("L" slime-load-file)
  ("n" slime-remove-notes)
  ("i" slime-interrupt)
  ("b" slime-eval-buffer)
  ("d" slime-eval-defun)
  ("v" slime-re-evaluate-defvar)
  ("u" slime-undefine-function)
  ("e" slime-eval-last-expression)
  ("r" slime-eval-region)
  ("R" slime-pprint-eval-region)
  ("J" slime-pprint-eval-last-expression)
  ("j" slime-eval-print-last-expression)
  ("i" slime-interactive-eval)
  ("l" slime-eval-last-expression-in-repl)
  ("I" slime-inspect)
  ("E" slime-edit-value))

(defhydra matcha-slime-mode (:color blue :hint nil)
  "

    Slime: %s(buffer-name)

    Open                 Do                          Connection
  ------------------------------------------------------------------------------
    _x_ Scratch       _e_ Eval                _ll_ List Connections
    _ss_ Slime        _mo_ Macroexpand One    _ln_ Next Connection
    _sq_ Quit         _ma_ Macroexpand All    _lp_ Previous Connection
    _?_ Cheatsheet    _lt_ List Threads
    _z_ REPL

      Who                        Help                     Misc
  ------------------------------------------------------------------------------
    _wr_ References       _aa_ Apropos            _Af_ Trace Function
    _wm_ Macroexpands     _al_ Apropos All        _Au_ Untrace All
    _wl_ Specializes      _ap_ Apropos Package    _Af_ Toggle Fancy Trace
    _wc_ Calls            _h_ Describe Symbol     _gn_ Next Note
    _wb_ Binds            _H_ Hyperspec Lookup    _gp_ Previous Note
    _ww_ Calls Who        _d_ Disassemble
    _lc_ List Callers
    _le_ List Callees

"
  ("z" slime-repl)
  ("lt" slime-list-threads)
  ("ln" slime-next-connection)
  ("lp" slime-prev-connection)
  ("ll" slime-list-connections)
  ("?" slime-cheat-sheet)
  ("gn" slime-next-note)
  ("gp" slime-previous-note)
  ("aa" slime-apropos)
  ("al" slime-apropos-all)
  ("d" slime-disassemble-symbol)
  ("h" slime-describe-symbol)
  ("H" slime-hyperspec-lookup)
  ("ap" slime-apropos-package)
  ("Af" slime-toggle-trace-fdefinition)
  ("Au" slime-untrace-all)
  ("wr" slime-who-references)
  ("wm" slime-who-macroexpands)
  ("wl" slime-who-specializes)
  ("wc" slime-who-calls)
  ("ww" slime-calls-who)
  ("wb" slime-who-binds)
  ("ws" slime-who-sets)
  ("lc" slime-list-callers)
  ("le" slime-list-callees)
  ("x" slime-scratch)
  ("ss" slime)
  ("sq" slime-quit-lisp)
  ("e" matcha-slime-eval/body)
  ("ma" slime-macroexpand-all)
  ("mo" slime-macroexpand-1)
  ("Af" slime-toggle-fancy-trace))

(defun matcha-slime-set-launcher ()
  "Set `hydra' launcher for `slime'."
  (matcha-set-mode-command
   :mode 'slime-mode :command #'matcha-slime-mode/body :minor-p t)
  (matcha-set-eval-command
   :mode 'slime-mode :command #'matcha-slime-eval/body :minor-p t))

(provide 'matcha-slime)
;;; matcha-slime.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
