(require 'hydra-integration-base)
;; (require 'prettier-js)

(defun +prettier-or-indent-region-or-buffer ()
  "Format with `prettier-js' or use `indent-region-or-buffer'."
  (interactive)
  (cond
   ((null buffer-file-name)
    (indent-region-or-buffer))
   ((and (eq major-mode 'web-mode)
         buffer-file-name
         (or (string-match "\\.jsx?\\'" buffer-file-name)
             (string-match "\\.tsx?\\'" buffer-file-name)))
    (prettier-js))
   ((executable-find "prettier")
    (prettier-js))
   (:default
    (indent-region-or-buffer))))

(provide 'hydra-prettier)
