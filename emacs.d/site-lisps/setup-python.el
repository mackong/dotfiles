;;; setup-python.el --- setup for python development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for python development.

;;; Code:

(require 'lsp-mode)

(defun setup-python-mode ()
  "Setup for python mode."
  (lsp-define-stdio-client lsp-python "python"
                         #'projectile-project-root
                         '("pyls"))

  (lsp-python-enable)
  (sphinx-doc-mode t)
  (define-key python-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (python-shell-send-buffer t))))

(add-hook 'python-mode-hook 'setup-python-mode)

(provide 'setup-python)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-python.el ends here
