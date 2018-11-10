;;; matcha-geiser.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'geiser)

(defmacro matcha-geiser-define-eval-and-print-functions (&rest geiser-evals)
  "Define eval functions that prints to the buffer after."
  `(progn
     ,@(cl-loop
        for eval in geiser-evals
        collect
        (let ((funsymbol (intern (concat (symbol-name eval) "-and-print"))))
          `(defun ,funsymbol ()
             ,(concat "Run " (symbol-name eval) " and print to buffer.")
             (interactive)
             (let ((current-prefix-arg 4)) ; C-u
               (call-interactively ',eval)))))))

(matcha-geiser-define-eval-and-print-functions geiser-eval-last-sexp)

;; TODO geiser-debug
;; TODO geiser-company
;; TODO geiser-repl binds

(defhydra matcha-geiser-insert (:color blue :columns 4)
  "Insert"
  ("." geiser-completion--complete-module "Complete Module")
  ("i" completion-at-point "Completion at Point")
  ("l" geiser-insert-lambda "Insert Lambda"))

(defhydra matcha-geiser-eval (:color blue :columns 4)
  "Eval"
  ("p" geiser-eval-last-sexp-and-print "S-exp and Print")
  ("e" geiser-eval-last-sexp "S-exp")
  ("b" geiser-eval-buffer "Buffer")
  ("d" geiser-eval-definition "Definition")
  ("r" geiser-eval-region "Region")
  ("k" geiser-compile-current-buffer "Compile Buffer")
  ("l" geiser-load-file "Load File")
  ("B" geiser-eval-buffer-and-go "Buffer and Go")
  ("D" geiser-eval-definition-and-go "Definition and Go")
  ("R" geiser-eval-region-and-go "Region and Go")
  ("xe" geiser-expand-last-sexp "Expand S-exp")
  ("xr" geiser-expand-region "Expand Region")
  ("xd" geiser-expand-definition "Expand Definition"))

(defhydra matcha-geiser-navigate (:color blue :columns 4)
  "Navigate"
  ("g" geiser-edit-symbol-at-point "Find Definition")
  ("b" geiser-pop-symbol-stack "Find Definition Pop")
  ("r" geiser-xref-callers "Callers")
  ("R" geiser-xref-callees "Callees"))

;; TODO: Macroexpand bindings.

(defhydra matcha-geiser-mode (:color blue :columns 4)
  "Geiser"
  ("c" geiser-compile-current-buffer "Compile Buffer")
  ("g" matcha-geiser-navigate/body "Navigate")
  ("h" matcha-geiser-help/body "Help")
  ("e" matcha-geiser-eval/body "Eval")
  ("i" matcha-geiser-insert/body "Insert")
  ("m" matcha-geiser-misc/body "Misc")
  ("s" matcha-geiser-scheme/body "Scheme")
  ("k" geiser-restart-repl "Restart Repl")
  ("j" run-geiser "Run Geiser"))

(defhydra matcha-geiser-help (:color blue :columns 4)
  "Help"
  ("r" geiser-xref-callers "Callers")
  ("R" geiser-xref-callees "Callees")
  ("k" geiser-doc-symbol-at-point "Doc at Point")
  ("l" geiser-doc-look-up-manual "Look up Manual")
  ("m" geiser-doc-module "Doc Module")
  ("s" geiser-autodoc-show "Autodoc Show"))

(defhydra matcha-geiser-misc (:color blue :columns 4)
  "Misc"
  ("s" geiser-set-scheme "Set Scheme")
  ("l" geiser-add-to-load "Add to Load Path")
  ("e" geiser-edit-module "Edit Module")
  ("q" geiser-squarify "Squarify"))

(defhydra matcha-geiser-scheme (:color blue :columns 4)
  "Scheme"
  ("d" scheme-send-definition "Send Definition")
  ("c" scheme-compile-definition-and-go "Compile Definition and Go")
  ("k" scheme-compile-file "Compile File")
  ("l" scheme-load-file "Load File")
  ("r" scheme-send-region "Send Region")
  ("t" scheme-trace-procedure "Trace Procedure")
  ("x" scheme-expand-current-form "Expand Current Form")
  ("z" switch-to-scheme "Switch to Scheme")
  ("e" scheme-send-last-sexp "Send Last S-exp")
  ("c" scheme-compile-definition "Compile Definition")
  ("r" scheme-send-region-and-go "Send Region and Go"))

(defun matcha-geiser-set-launcher ()
  "Set up `hydra' launcher for `geiser'."
  (matcha-set-mode-command
   :mode 'geiser-mode :command #'matcha-geiser-scheme/body :minor-p t)
  (matcha-set-eval-command
   :mode 'geiser-mode :command #'matcha-geiser-eval/body :minor-p t))

(provide 'matcha-geiser)
;;; matcha-geiser.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
