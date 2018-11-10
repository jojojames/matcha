(require 'matcha-base)
;; (require 'js2-refactor nil t)

(defhydra matcha-js2-refactor (:color blue :hint nil)
  "
    Javascript Refactor: %(matcha-heading-current-file)

    ^Functions^                    ^Variables                  ^Debugging^
  ------------------------------------------------------------------------------
    _lp_ Localize Parameter        _ev_ Extract variable     _lt_ Log this
    _ef_ Extract function          _iv_ Inline variable      _dt_ Debug this
    _ip_ Introduce parameter       _rv_ Rename variable
    _em_ Extract method            _vt_ Var to this
    _ao_ Arguments to object       _sv_ Split var decl.
    _tf_ Toggle fun exp and decl   _ag_ Add var to globals
    _ta_ Toggle fun expr and =>    _ti_ Ternary to if
                                 ^^_wl_ Wrap in For Loop

    ^Sexp^                   ^Buffer^
  ------------------------------------------------------------------------------
    _k_  JS2 Kill          _wi_ Wrap buffer in IIFE
    _ss_ Split String      _ig_ Inject global in IIFE
    _sl_ Forward Slurp     _ee_ Expand node at point
    _ba_ Forward Barf      _cc_ Contract node at point
                         ^^_uw_ Unwrap

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

(provide 'matcha-js2-refactor)
