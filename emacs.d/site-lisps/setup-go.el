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
  (setq tab-width 4)

  ;; use goimports for go-fmt
  (setq gofmt-command "goimports")

  ;; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (require 'lsp-clients)
  (condition-case nil
      (lsp)
    (error nil)))

(add-hook 'go-mode-hook 'setup-go-mode)


;; go-playground
(setq go-playground-basedir "~/Codes/go/playground"
      go-playground-confirm-deletion nil)
(global-set-key (kbd "C-c C-p n") 'go-playground)
(global-set-key (kbd "C-c C-p r") 'go-playground-rm)

(provide 'setup-go)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-go ends here
