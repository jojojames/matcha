;;; matcha-elisp.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(defconst matcha-elisp-fns-regexp
  (concat "(\\s-*"
          "\\(defun\\|defmacro\\)\\s-+"
          "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>")
  "Regexp to find defun or defmacro definition.")

(defun matcha-elisp-toggle-edebug-defun ()
  (interactive)
  (let (fn)
    (save-mark-and-excursion
      (search-backward-regexp matcha-elisp-fns-regexp)
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

(when matcha-use-launcher-p
  (matcha-set-format-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-indent-region-or-buffer)
  (matcha-set-debug-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-debug)
  (matcha-set-eval-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-eval)
  (matcha-set-mode-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'matcha-emacs-lisp-mode)
  (matcha-set-refactor-command
   :mode '(emacs-lisp-mode lisp-interaction-mode)
   :command 'emr-show-refactor-menu))

(define-transient-command matcha-emacs-lisp-debug
  "Debug"
  [["Debug"
    ("d" "Debug" matcha-elisp-toggle-edebug-defun)
    ("q" "Cancel Debug on Entry" cancel-debug-on-entry)
    ("f" "Debug on Entry" debug-on-entry)]
   ["Watch"
    ("w" "Watch" debug-watch)
    ("W" "Cancel Watch" cancel-debug-watch)]])

(define-transient-command matcha-emacs-lisp-eval ()
  "Eval"
  [["Eval"
    ("e" "Last" eval-last-sexp)
    ("r" "Region" eval-region)
    ("f" "Defun" eval-defun)
    ("b" "Buffer" eval-buffer)]
   ["Current"
    ("c" "Form" matcha-elisp-eval-current-form-sp)
    ("s" "Symbol" matcha-elisp-eval-current-symbol-sp)]
   ["Misc"
    ("j" "Eval and Print" eval-print-last-sexp)]])

(define-transient-command matcha-elisp-refs ()
  "References"
  [["References"
    ("f" "Function" elisp-refs-function)
    ("m" "Macro" elisp-refs-macro)
    ("c" "Special" elisp-refs-special)
    ("v" "Variable" elisp-refs-variable)
    ("s" "Symbol" elisp-refs-symbol)]])

(define-transient-command matcha-emacs-lisp-compile ()
  "Compile"
  [["Compile"
    ("c" "Compile" emacs-lisp-byte-compile)
    ("l" "Compile and Load" emacs-lisp-byte-compile-and-load)
    ("r" "Byte Recompile Directory" byte-recompile-directory)
    ("x" "Disassemble" disassemble)]])

(define-transient-command matcha-emacs-lisp-describe ()
  "Describe"
  [["Describe"
    ("f" "Function" describe-function)
    ("v" "Variable" describe-variable)
    ("s" "Symbol" describe-symbol)
    ("y" "Syntax" describe-syntax)
    ("c" "Categories" describe-categories)]])

(define-transient-command matcha-emacs-lisp-mode ()
  "Emacs Lisp"
  [["Actions"
    ("c" "Compile..." matcha-emacs-lisp-compile)
    ("d" "Debug..." matcha-emacs-lisp-debug)
    ("e" "Eval..." matcha-emacs-lisp-eval)
    ("m" "Macroexpand..." matcha-macrostep-expand-or-open-menu)
    ("s" "Describe..." matcha-emacs-lisp-describe)]
   ["Find"
    ("v" "Variable" find-variable)
    ("f" "Function" find-function)
    ("l" "Library" find-library)
    ("r" "References..." matcha-elisp-refs)]
   ["PP"
    ("pm" " Macroexpand Expression..." pp-macroexpand-expression)
    ("pl" " Macroexpand Last S-exp..." pp-macroexpand-last-sexp)
    ("pb" "Prettify Current Buffer" pp-buffer)
    ("pe" "Eval Last Sexp" pp-eval-last-sexp)
    ("ps" "Prettify Expression" pp-eval-expression)]
   ["Misc"
    ("x" "*Scratch*" matcha-goto-scratch)
    ("z" "IELM" ielm)]])

(provide 'matcha-elisp)
;;; matcha-elisp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
