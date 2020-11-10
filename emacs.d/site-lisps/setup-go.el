;;; setup-go.el --- setup for golang development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for golang development.

;;; Code:

(defun setup-go-mode ()
  "Setup for go mode."
  (setq-local tab-width 4)
  (whitespace-mode -1)

  ;; use goimports for go-fmt
  (setq gofmt-command "goimports")

  ;; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (condition-case nil
      (progn
        (setq lsp-go-hover-kind "FullDocumentation")
        (lsp))
    (error nil)))

(add-hook 'go-mode-hook 'setup-go-mode)

(provide 'setup-go)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-go ends here
