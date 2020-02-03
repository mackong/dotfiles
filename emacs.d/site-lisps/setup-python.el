;;; setup-python.el --- setup for python development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for python development.

;;; Code:

(defun setup-python-mode ()
  "Setup for python mode."
  (condition-case nil
      (lsp)
    (error nil))

  (sphinx-doc-mode t)
  (define-key python-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (python-shell-send-buffer t))))

(add-hook 'python-mode-hook 'setup-python-mode)

(pyvenv-workon (car (seq-filter '(lambda (x) (equal "daily" (car (split-string x "-")))) (pyvenv-virtualenv-list))))

(provide 'setup-python)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-python.el ends here
