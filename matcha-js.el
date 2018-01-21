(require 'matcha-base)
(require 'matcha-mocha)
(require 'matcha-prettier)
(require 'matcha-indium)
(require 'matcha-js2-refactor)
(require 'rjsx-mode nil t)

(defun matcha-js-set-launcher ()
  "Set `hydra' launcher for `js'."
  (matcha-set-refactor-command
   :mode '(js-mode js2-mode rjsx-mode) :command #'matcha-js2-refactor/body)
  (matcha-set-debug-command
   :mode '(js-mode js2-mode rjsx-mode) :command #'matcha-indium-debug/body)

  (matcha-set-eval-command
   :mode '(js-mode js2-mode rjsx-mode) :command #'matcha-indium-eval/body)

  (matcha-set-mode-command
   :mode '(js-mode js2-mode rjsx-mode) :command #'matcha-indium-mode/body)

  (matcha-set-test-command
   :mode '(js-mode js2-mode rjsx-mode) :command #'matcha-mocha/body)

  (matcha-set-format-command
   :mode '(js-mode js2-mode rjsx-mode)
   :command #'matcha-prettier-or-indent-region-or-buffer))

(provide 'matcha-js)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
