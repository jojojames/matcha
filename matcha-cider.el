;;; matcha-cider.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'matcha-clj-refactor)
(require 'cider nil t)

;; https://gist.github.com/ddellacosta/4967694b765b91e9d687
(defun cider-jack-in-with-profile (profile)
  "Starts up a cider repl using jack-in with the specific lein profile
   selected."
  (interactive "sProfile: ")
  (let* ((profile-str profile)
         (profile-str (replace-regexp-in-string ":\\(.*\\)$" "\\1" profile-str))
         (lein-params (concat "with-profile +" profile-str " repl :headless")))
    (setq cider-lein-parameters lein-params)
    (cider-jack-in)))

(defun cider-jack-in-with-profile-test ()
  "Wrapper over `cider-jack-in-with-profile' to run test environment."
  (interactive)
  (cider-jack-in-with-profile "test"))

(defun cider-jack-in-dont-auto-run ()
  "Call cider-jack-in but don't call cider-run aftewards.
This method assumes we've done something like this below.
`(add-hook 'cider-connected-hook #'cider-run)'"
  (interactive)
  ;; Remove the hook before running `cider-jack-in'.
  (remove-hook 'cider-connected-hook #'cider-run)
  ;; Add it back after we're connected.
  (add-hook 'cider-connected-hook
            (lambda ()
              (add-hook 'cider-connected-hook #'cider-run)))
  (cider-jack-in))

(defun matcha-cider-connect-dwim ()
  "Connect with cider-jack-in if not connected or restart if it is."
  (interactive)
  (if (cider-connected-p)
      (cider-restart)
    (cider-jack-in)))

(defmacro matcha-cider-define-lein-command (&rest lein-commands)
  "Create a command that interfaces with lein.
If the command contains a %s, it will be an interactive command that asks
for user input.
\"run\" becomes `lein-run'.
\"migratus create %s\" becomes `lein-migratus-create-%s'."
  `(progn
     ,@(cl-loop
        for command in lein-commands
        collect
        (let* ((command-tokens (split-string command))
               (funsymbol
                (intern (concat "lein-"
                                (mapconcat (lambda (x) x)
                                           command-tokens
                                           "-")))))
          `(defun ,funsymbol (&optional arg)
             ,(concat "Run $ lein " command ".")
             ,(if (string-equal "%s" (car (last command-tokens)))
                  '(interactive "sEnter arg: ")
                '(interactive))
             (let ((clj-command ,command)
                   (default-directory (projectile-project-root)))
               (compilation-start
                (if arg
                    (concat "lein " (format clj-command arg))
                  (concat "lein " clj-command))
                'compilation-mode (lambda (_) "*lein*") t)))))))

(matcha-cider-define-lein-command "run"
                                  "migratus migrate"
                                  "migratus rollback"
                                  "migratus down"
                                  "migratus up"
                                  "migratus reset"
                                  "migratus create %s")

(defhydra matcha-cider-lein
  (:color blue :columns 4)
  "Lein"
  ("j" lein-run "Run")
  ("m" lein-migratus-migrate "Migrate")
  ("r" lein-migratus-rollback "Rollback")
  ("d" lein-migratus-down "Down")
  ("u" lein-migratus-up "Up")
  ("R" lein-migratus-reset "Reset")
  ("c" lein-migratus-create-%s "Create"))

(defhydra matcha-cider-doc (:color blue)
  "

    Cider Documentation
  ------------------------------------------------------------------------------
    _d_ CiderDoc               _j_ JavaDoc in browser
    _a_ Search symbols         _s_ Search symbols & select
    _A_ Search documentation   _e_ Search documentation & select
    _r_ Grimoire               _h_ Grimoire in browser

"
  ;; CiderDoc
  ("d" cider-doc nil)
  ;; JavaDoc
  ("j" cider-javadoc nil)
  ;; Apropos
  ("a" cider-apropos nil)
  ("s" cider-apropos-select nil)
  ("A" cider-apropos-documentation nil)
  ("e" cider-apropos-documentation-select nil)
  ;; Grimoire
  ("r" cider-grimoire nil)
  ("h" cider-grimoire-web nil))

(defhydra matcha-cider-eval (:color blue :hint nil)
  "

    Cider Evaluation

       Eval                 Eval and Print        Load
  ------------------------------------------------------------------------------
    _e_ S-exp               _p_ S-exp          _k_ Buffer
    _w_ S-exp and Replace   _f_ Defun          _l_ File
    _d_ Defun at Point      _i_ Inspect        _p_ Project Namespaces
    _r_ Region
    _n_ NS Form
    _b_ Buffer
    _:_ Read and Eval

      Macroexpand                 REPL
  ------------------------------------------------------------------------------
    _m_ Macroexpand-1      _z_ Switch to REPl
    _M_ Macroexpand all    _I_ Insert S-exp in REPL

"
  ("z" cider-switch-to-repl-buffer)
  ("I" cider-insert-last-sexp-in-repl)
  ("b" cider-eval-buffer)
  ;; Load
  ("k" cider-load-buffer nil)
  ("l" cider-load-file nil)
  ("p" cider-load-all-project-ns nil)
  ;; Eval
  ("r" cider-eval-region nil)
  ("n" cider-eval-ns-form nil)
  ("e" cider-eval-last-sexp nil)
  ("p" cider-pprint-eval-last-sexp nil)
  ("w" cider-eval-last-sexp-and-replace nil)
  ("E" cider-eval-last-sexp-to-repl nil)
  ("d" cider-eval-defun-at-point nil)
  ("f" cider-pprint-eval-defun-at-point nil)
  (":" cider-read-and-eval nil)
  ;; Inspect
  ("i" cider-inspect nil)
  ;; Macroexpand
  ("m" cider-macroexpand-1 nil)
  ("M" cider-macroexpand-all nil))

(defhydra matcha-cider-test (:color blue :hint nil)
  "

    Cider Test
  ------------------------------------------------------------------------------
    _t_ Run Test           _r_ Rerun Test
    _n_ Run NS Tests       _l_ Run Loaded Tests
    _p_ Run Project Tests  _b_ Show Report

"
  ("t" cider-test-run-test)
  ("r" cider-test-rerun-tests)
  ("n" cider-test-run-ns-tests)
  ("l" cider-test-run-loaded-tests)
  ("p" cider-test-run-project-tests)
  ("b" cider-test-show-report))

(defhydra matcha-cider-debug (:color blue)
  "Debug"
  ("f" cider-debug-defun-at-point "Defun at Point")
  ("d" cider-debug-defun-at-point "Defun at Point")
  ("L" cider-browse-instrumented-defs "Browse Instrumented Defs")
  ("l" cider-debug-toggle-locals "Toggle Locals"))

(defhydra matcha-cider-repl (:color blue)
  "

    Cider REPL
  ------------------------------------------------------------------------------
    _d_ Display connection info          _r_ Rotate default connection
    _z_ Switch to REPL                   _n_ Set REPL ns
    _p_ Insert last sexp in REPL         _x_ Reload namespaces
    _o_ Clear REPL output                _O_ Clear entire REPL
    _b_ Interrupt pending evaluations    _Q_ Quit CIDER

"
  ;; Connection
  ("d" cider-display-connection-info nil)
  ("r" cider-rotate-default-connection nil)
  ;; Input
  ("z" cider-switch-to-repl-buffer nil)
  ("n" cider-repl-set-ns nil)
  ("p" cider-insert-last-sexp-in-repl nil)
  ("x" cider-refresh nil)
  ;; Output
  ("o" cider-find-and-clear-repl-output nil)
  ("O" (lambda () (interactive) (cider-find-and-clear-repl-output t)) nil)
  ;; Interrupt/quit
  ("b" cider-interrupt nil)
  ("Q" cider-quit nil))

(defhydra matcha-cider-mode (:color blue :hint nil)
  "

    Cider: %s(matcha-projectile-root)

        Do               Jack In                   Connections
  ------------------------------------------------------------------------------
    _e_ Eval       _jj_ J.I.                  _cc_ Connect To
    _t_ Test       _ju_ J.I. -No Autorun      _cd_ Connect DWIM
    _d_ Debug      _jp_ J.I. -With Profile    _cr_ Rotate Connection
    _r_ Refactor   _jt_ J.I. -With Test       _cb_ Connection Browser
    _k_ Doc        _js_ J.I. -ClojureScript
    _z_ REPL       _A_ Run -main

         Misc                    ^^Macroexpand
  ------------------------------------------------------------------------------
    _x_ Scratch                  ^^_m_ Macroexpand-1
    _R_ Reload Namespaces        ^^_M_ Macroexpand all
    _l_ Lein
    _q_ Quit

"

  ("z" matcha-cider-repl/body)
  ("k" matcha-cider-doc/body)
  ("cc" cider-connect)
  ("x" cider-scratch)
  ("e" matcha-cider-eval/body)
  ("t" matcha-cider-test/body)
  ("d" matcha-cider-debug/body)
  ("l" matcha-cider-lein/body)
  ("r" matcha-clj-refactor/body)
  ("cr" cider-rotate-default-connection)
  ("cb" cider-connection-browser)
  ("jj" cider-jack-in)
  ("js" cider-jack-in-clojurescript)
  ("ju" cider-jack-in-dont-auto-run)
  ("jp" cider-jack-in-with-profile)
  ("jt" cider-jack-in-with-profile-test)
  ("R" cider-refresh)
  ("cd" matcha-cider-connect-dwim)
  ("A" cider-run)
  ("q" cider-quit)
  ("m" cider-macroexpand-1)
  ("M" cider-macroexpand-all))

(defun matcha-cider-set-launcher ()
  "Set `hydra' launcher for `cider'."
  (let ((modes '(cider-mode cider-repl-mode cider-clojure-interaction-mode)))
    (matcha-set-debug-command :mode modes :command 'matcha-cider-debug/body)
    (matcha-set-eval-command :mode modes :command 'matcha-cider-eval/body)
    (matcha-set-mode-command :mode modes :command 'matcha-cider-mode/body)
    (matcha-set-test-command :mode modes :command 'matcha-cider-test/body)
    (matcha-set-refactor-command :mode modes :command 'matcha-clj-refactor/body)))

(provide 'matcha-cider)
;;; matcha-cider.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
