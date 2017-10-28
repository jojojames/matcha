(require 'hydra-integration-base)
(require 'hydra-mocha)
(require 'hydra-prettier)
;; (require 'indium)
;; (require 'js2-refactor)
;; (require 'js-import)
;; (require 'rjsx-mode)

(defhydra hydra-js-import (:color blue)
  "Import"
  ("i" js-import "Import")
  ("d" js-import-dev "Import Dev"))

(defhydra hydra-js2-refactor (:color blue :hint nil)
  "
    Javascript Refactor: %(+hydra-heading-current-file)

    ^Functions^                    ^Variables                  ^Debugging^
  ------------------------------------------------------------------------------
    [_lp_] Localize Parameter      [_ev_] Extract variable     [_lt_] Log this
    [_ef_] Extract function        [_iv_] Inline variable      [_dt_] Debug this
    [_ip_] Introduce parameter     [_rv_] Rename variable
    [_em_] Extract method          [_vt_] Var to this
    [_ao_] Arguments to object     [_sv_] Split var decl.
    [_tf_] Toggle fun exp and decl [_ag_] Add var to globals
    [_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
                                 ^^[_wl_] Wrap in For Loop

    ^Sexp^                   ^Buffer^
  ------------------------------------------------------------------------------
    [_k_]  JS2 Kill          [_wi_] Wrap buffer in IIFE
    [_ss_] Split String      [_ig_] Inject global in IIFE
    [_sl_] Forward Slurp     [_ee_] Expand node at point
    [_ba_] Forward Barf      [_cc_] Contract node at point
                           ^^[_uw_] Unwrap

"
  ("ee" js2r-expand-node-at-point)
  ("cc" js2r-contract-node-at-point)
  ("ef" js2r-extract-function)
  ("em" js2r-extract-method)
  ("tf" js2r-toggle-function-expression-and-declaration)
  ("ta" js2r-toggle-arrow-function-and-expression)
  ("ip" js2r-introduce-parameter)
  ("lp" js2r-localize-parameter)
  ("wi" js2r-wrap-buffer-in-iife)
  ("ig" js2r-inject-global-in-iife)
  ("ag" js2r-add-to-globals-annotation)
  ("ev" js2r-extract-var)
  ("iv" js2r-inline-var)
  ("rv" js2r-rename-var)
  ("vt" js2r-var-to-this)
  ("ao" js2r-arguments-to-object)
  ("ti" js2r-ternary-to-if)
  ("sv" js2r-split-var-declaration)
  ("ss" js2r-split-string)
  ("uw" js2r-unwrap)
  ("lt" js2r-log-this)
  ("dt" js2r-debug-this)
  ("sl" js2r-forward-slurp)
  ("ba" js2r-forward-barf)
  ("k" js2r-kill)
  ("wl" js2r-wrap-in-for-loop))

(defhydra hydra-indium-eval (:color blue :columns 4)
  "Eval"
  ("e" indium-eval-last-node "Expression")
  ("f" indium-eval-defun "Function")
  ("i" indium-inspect-last-node "Inspect Last Node")
  ("b" indium-eval-buffer "Buffer"))

(defhydra hydra-indium-debug (:color blue :columns 4)
  "Debug"
  ("b" indium-add-breakpoint "Add Breakpoint")
  ("d" indium-add-breakpoint "Add Breakpoint")
  ("c" indium-add-conditional-breakpoint "Add conditional Breakpoint")
  ("k" indium-remove-breakpoint "Remove Breakpoint")
  ("x" indium-remove-breakpoint "Remove Breakpoint")
  ("K" indium-remove-all-breakpoints-from-buffer "Remove all Breakpoints")
  ("X" indium-remove-all-breakpoints-from-buffer "Remove all Breakpoints")
  ("a" indium-activate-breakpoints "Activate Breakpoints")
  ("D" indium-deactivate-breakpoints "Deactivate Breakpoints")
  ("l" indium-list-breakpoints "List Breakpoints"))

(defhydra hydra-indium-mode (:color blue :columns 4)
  "Javascript"
  ("t" hydra-js2-test/body "Test")
  ("i" hydra-js-import/body "Import")
  ("x" indium-run-node "Run Node")
  ("c" indium-scratch "Scratch")
  ("d" hydra-js2-debug/body "Debug")
  ("e" hydra-js2-eval/body "Eval")
  ("r" hydra-js2-refactor/body)
  ("z" indium-switch-to-repl-buffer "Switch to REPL")
  ("k" indium-update-script-source "Update Source"))

(+add-major-debug-command 'hydra-indium-debug/body
                          '(js2-mode rjsx-mode))

(+add-major-eval-command 'hydra-indium-eval/body
                         '(js2-mode rjsx-mode))

(+add-major-mode-command 'hydra-indium-mode/body
                         '(js2-mode rjsx-mode))

(+add-major-test-command #'hydra-mocha/body
                         '(js2-mode rjsx-mode))

(+add-major-indent-command #'+prettier-or-indent-region-or-buffer
                           '(js-mode
                             js2-mode
                             rjsx-mode))

(provide 'hydra-javascript)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
