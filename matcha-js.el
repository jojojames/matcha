(require 'matcha-base)
(require 'matcha-mocha)
(require 'matcha-prettier)
(require 'matcha-indium)
(require 'matcha-js2-refactor)
(require 'rjsx-mode nil t)

(defun matcha-js-set-launcher ()
  "Set `hydra' launcher for `js'."
  (matcha-add-major-debug-command 'matcha-indium-debug/body
                                  '(js2-mode rjsx-mode))

  (matcha-add-major-eval-command 'matcha-indium-eval/body
                                 '(js2-mode rjsx-mode))

  (matcha-add-major-mode-command 'matcha-indium-mode/body
                                 '(js2-mode rjsx-mode))

  (matcha-add-major-test-command #'matcha-mocha/body
                                 '(js2-mode rjsx-mode))

  (matcha-add-major-format-command #'matcha-prettier-or-indent-region-or-buffer
                                   '(js-mode
                                     js2-mode
                                     rjsx-mode)))

(provide 'matcha-js)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
