;;; setup-scala.el --- setup for scala development

;; Copyright (C) 2019 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for scala development.

;;; Code:

(defun setup-scala-mode ()
  "Setup for scala mode."
  (require 'lsp-clients)
  (condition-case nil
      (lsp)
    (error nil)))

(add-hook 'scala-mode-hook 'setup-scala-mode)

(provide 'setup-scala)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-scala ends here
