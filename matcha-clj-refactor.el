;;; matcha-clj-refactor.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'clj-refactor nil t)

;; Redefine hydras to be consistent.
;; This will be a maintenance burden but better to be consistent...
;; These hydras are defined in `clj-refactor'.
(defhydra matcha-cljr-ns-menu (:color blue :hint nil)
  "

    Ns related refactorings
  ------------------------------------------------------------------------------
    _ai_ Add Import               _cn_ Clean ns
    _ar_ Add Require              _sr_ Stop Referring
    _rm_ Require a Macro
    _am_ Add Missing Libspec
    _au_ Add Use to ns
    _ap_ Add Project Dependency

"
  ("ai" cljr-add-import-to-ns) ("am" cljr-add-missing-libspec)
  ("ap" cljr-add-project-dependency) ("ar" cljr-add-require-to-ns)
  ("au" cljr-add-use-to-ns) ("cn" cljr-clean-ns)
  ("rm" cljr-require-macro) ("sr" cljr-stop-referring))

(defhydra matcha-cljr-code-menu (:color blue :hint nil)
  "

    Code related refactorings
  ------------------------------------------------------------------------------
    _ci_ Cycle if          _ct_ Cycle thread
    _dk_ Destructure keys  _el_ Expand let     _fu_ Find usages
    _il_ Introduce let     _is_ Inline symbol  _ml_ Move to let
    _pf_ Promote function  _rl_ Remove let     _rs_ Rename symbol
    _tf_ Thread first all  _th_ Thread         _tl_ Thread last all
    _ua_ Unwind all        _uw_ Unwind

"
  ("ci" clojure-cycle-if) ("ct" cljr-cycle-thread)
  ("dk" cljr-destructure-keys) ("el" cljr-expand-let)
  ("fu" cljr-find-usages) ("il" cljr-introduce-let)
  ("is" cljr-inline-symbol) ("ml" cljr-move-to-let)
  ("pf" cljr-promote-function) ("rl" cljr-remove-let)
  ("rs" cljr-rename-symbol) ("tf" clojure-thread-first-all)
  ("th" clojure-thread) ("tl" clojure-thread-last-all)
  ("ua" clojure-unwind-all) ("uw" clojure-unwind))

(defhydra matcha-cljr-project-menu (:color blue :hint nil)
  "

    Project related refactorings
  ------------------------------------------------------------------------------
    _ap_ Add Project dependency         _cs_ Change function signature
    _hd_ Hotload dependency             _is_ Inline symbol
    _pc_ Project clean                  _rf_ Rename file-or-dir
    _up_ Update Project dependencies    _rs_ Rename symbol
    _sp_ Sort Project dependencies      _mf_ Move form
                                        ^^_fu_ Find Usages

"
  ("ap" cljr-add-project-dependency) ("cs" cljr-change-function-signature)
  ("fu" cljr-find-usages) ("hd" cljr-hotload-dependency)
  ("is" cljr-inline-symbol) ("mf" cljr-move-form)
  ("pc" cljr-project-clean) ("rf" cljr-rename-file-or-dir)
  ("rs" cljr-rename-symbol) ("sp" cljr-sort-project-dependencies)
  ("up" cljr-update-project-dependencies))

(defhydra matcha-cljr-toplevel-form-menu (:color blue :hint nil)
  "

    Toplevel form related refactorings
  ------------------------------------------------------------------------------
    _ef_ Extract function            _cp_ Cycle privacy
    _ec_ Extract constant            _cs_ Change function signature
    _ed_ Extract form as def         _is_ Inline symbol
    _pf_ Promote function            _ad_ Add declaration
    _rf_ Rename file-or-dir          _mf_ Move form
    _fe_ Create function from example
    _as_ Add stubs for the interface/protocol at point

"
  ("as" cljr-add-stubs) ("cp" clojure-cycle-privacy)
  ("cs" cljr-change-function-signature) ("ec" cljr-extract-constant)
  ("ed" cljr-extract-def) ("ef" cljr-extract-function)
  ("fe" cljr-create-fn-from-example) ("is" cljr-inline-symbol)
  ("mf" cljr-move-form) ("pf" cljr-promote-function)
  ("rf" cljr-rename-file-or-dir) ("ad" cljr-add-declaration))

(defhydra matcha-cljr-cljr-menu (:color blue :hint nil)
  "

    Cljr related refactorings
  ------------------------------------------------------------------------------
    _sc_ Show the project's changelog
    _?_ Describe refactoring

"
  ("sc" cljr-show-changelog)
  ("?" cljr-describe-refactoring))

(defhydra matcha-clj-refactor (:color blue :hint nil)
  "

    Cider Refactor
  ------------------------------------------------------------------------------
    _n_ Namespaces    _c_ Code    _p_ Project
    _f_ Form          _s_ CLJR

"
  ("n" matcha-cljr-ns-menu/body)
  ("c" matcha-cljr-code-menu/body)
  ("p" matcha-cljr-project-menu/body)
  ("f" matcha-cljr-toplevel-form-menu/body)
  ("s" matcha-cljr-cljr-menu/body))

(provide 'matcha-clj-refactor)
;;; matcha-clj-refactor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
