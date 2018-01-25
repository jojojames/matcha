;;; matcha-elisp.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'matcha-macrostep)
;; (require 'elisp-refs)

(defun matcha-goto-scratch ()
  "Move to *scratch* buffer."
  (interactive)
  (pop-to-buffer "*scratch*"))

(defun matcha-elisp-eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (ignore evil-move-beyond-eol)
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun matcha-elisp-eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    (ignore evil-move-beyond-eol)
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

;; https://github.com/jwiegley/use-package/issues/152
;; Edebug a defun or defmacro
(defvar matcha-fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")
(defconst modi/fns-regexp (concat "(\\s-*"
                                  "\\(defun\\|defmacro\\)\\s-+"
                                  "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
  "Regexp to find defun or defmacro definition.")

(defun modi/toggle-edebug-defun ()
  (interactive)
  (let (fn)
    (save-mark-and-excursion
      (search-backward-regexp modi/fns-regexp)
      (setq fn (match-string 1))
      (mark-sexp)
      (narrow-to-region (point) (mark))
      (if (member fn matcha-fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq matcha-fns-in-edebug (delete fn matcha-fns-in-edebug))
            (eval-region (point) (mark))
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (progn
          (add-to-list 'matcha-fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn)))
      (widen))))

(defhydra matcha-emacs-lisp-debug (:color blue :hint nil)
  "

    Elisp Debug

    Debug                       ^^Watch              Edebug-X
  ------------------------------------------------------------------------------
    _d_ Debug                    _w_ Watch            _t_ Toggle Breakpoint
    _f_ Debug on Entry           _W_ Cancel Watch     _s_ Show Breakpoints
    _q_ Cancel Debug on Entry                       ^^_i_ Show Instrumented
                                                  ^^^^_a_ Show Data

"
  ("w" debug-watch)
  ("W" cancel-debug-watch)
  ("t" edebug-x-modify-breakpoint-wrapper)
  ("s" edebug-x-show-breakpoints)
  ("i" edebug-x-show-instrumented)
  ("a" edebug-x-show-data)
  ("q" cancel-debug-on-entry)
  ("f" debug-on-entry)
  ("d" modi/toggle-edebug-defun))

(defhydra matcha-emacs-lisp-eval (:color blue :hint nil)
  "

    Elisp Eval
  ------------------------------------------------------------------------------
    _e_ Last Sexp    _r_ Region    _f_ Defun    _b_ Buffer

    _c_ Current Form    _s_ Current Symbol

    _j_ Eval & Print    _x_ Eval & Replace

"
  ("e" eval-last-sexp)
  ("r" eval-region)
  ("f" eval-defun)
  ("b" eval-buffer)
  ("c" matcha-elisp-eval-current-form-sp)
  ("s" matcha-elisp-eval-current-symbol-sp)
  ("j" eval-print-last-sexp)
  ("x" eval-last-sexp-and-replace))

(defhydra matcha-emacs-lisp-mode (:color blue :hint nil)
  "

    Elisp: %s(matcha-projectile-root)

        Do                ^^Find        Describe^^
  ------------------------------------------------------------------------------
    _e_ Eval          _l_ Library        _sy_ Syntax
    _d_ Debug         _f_ Function       _sf_ Function
    _m_ Macrostep     _v_ Variable       _sv_ Variable
                                     ^^^^_ss_ Symbol
                                     ^^^^_sc_ Categories


        Compile                  References         Misc
  ------------------------------------------------------------------------------
     _u_ Byte Compile            _rf_ Functions     _x_ Scratch
     _U_ Byte Compile and Load   _rm_ Macros        _z_ REPL
    _rd_ Recompile Directory     _rc_ Special
    _rD_ Disassemble             _rv_ Variables
                               ^^_rs_ Symbols

"
  ("rf" elisp-refs-function)
  ("rm" elisp-refs-macro)
  ("rc" elisp-refs-special)
  ("rv" elisp-refs-variable)
  ("rs" elisp-refs-symbol)
  ("u" emacs-lisp-byte-compile)
  ("U" emacs-lisp-byte-compile-and-load)
  ("rd" byte-recompile-directory)
  ("rD" disassemble)
  ("x" matcha-goto-scratch)
  ("z" ielm)
  ("sf" describe-function)
  ("sv" describe-variable)
  ("ss" describe-symbol)
  ("sy" describe-syntax)
  ("sc" describe-categories)

  ("l" find-library)
  ("f" find-function)
  ("v" find-variable)
  ("m" matcha-macrostep-expand-or-open-hydra)
  ("e" matcha-emacs-lisp-eval/body)
  ("d" matcha-emacs-lisp-debug/body))

(when matcha-use-launcher-p
  (matcha-set-format-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-indent-region-or-buffer)
  (matcha-set-debug-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-debug/body)
  (matcha-set-eval-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-eval/body)
  (matcha-set-mode-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-mode/body)
  (matcha-set-refactor-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'emr-show-refactor-menu))

(provide 'matcha-elisp)
;;; matcha-elisp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
