;;; setup-cc.el --- setup for c/c++ development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for c/c++ development

;;; Code:

(defun setup-c-mode-common ()
  "Setup for cc-mode-common-hook."
  (c-toggle-hungry-state 1)
  (setq-local indent-tabs-mode nil)
  (setq-local c-basic-offset 8)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'innamespace 0))

(defun setup-c/c++-mode ()
  "Setup for c/c++ mode"
  (c-set-style "linux")

  (setup-c-mode-common)

  (setq gdb-many-windows t)
  (setq gdb-use-separate-io-buffer t)

  (if (not (string-equal major-mode "glsl-mode"))
      (condition-case nil
          (progn
            (setq lsp-clients-clangd-args '("--header-insertion=never"))
            (lsp))
        (error nil))))

(add-hook 'c-mode-hook 'setup-c/c++-mode)
(add-hook 'c++-mode-hook 'setup-c/c++-mode)

(provide 'setup-cc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-cc.el ends here
