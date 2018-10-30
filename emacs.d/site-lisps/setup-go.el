;;; setup-go.el --- setup for golang development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for golang development.

;;; Code:

(require 'lsp-go)

(defun setup-go-mode ()
  "Setup for go mode."
  (setq gdb-many-windows t)
  (setq tab-width 4)

  ;; use goimports for go-fmt
  (setq gofmt-command "goimports")
  ;; use gogetdoc for doc
  (setq godoc-at-point-function #'godoc-gogetdoc)

  ;; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (lsp-go-enable)

  (define-key go-mode-map (kbd "C-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-,") 'pop-tag-mark)
  (define-key go-mode-map (kbd "C-c C-j") 'helm-imenu)
  (define-key go-mode-map (kbd "C-c s p") 'go-set-project)
  (define-key go-mode-map (kbd "C-c C-k") 'godoc-at-point))

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
