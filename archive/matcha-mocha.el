(require 'matcha-base)
;; (require 'mocha)

(defhydra matcha-mocha (:color blue :hint nil)
  "

   Mocha: %s(matcha-projectile-root)

    Test               Debug
  ------------------------------------------------------------------------------
    _p_ Project    _dp_ Project
    _f_ File       _df_ File
    _t_ Point      _dt_ Point

"
  ("p" mocha-test-project)
  ("dp" mocha-debug-project)
  ("f" mocha-test-file)
  ("df" mocha-debug-file)
  ("t" mocha-test-at-point)
  ("dt" mocha-debug-at-point))

(provide 'matcha-mocha)
