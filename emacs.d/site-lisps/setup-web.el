;;; setup-web.el --- setup for web development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for web development.
;; Include html, javascript, css, php.

;;; Code:

(defun js-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my-js-hook nil
  (setq js-indent-level 2)

  (js2-minor-mode)

  (require 'lsp-clients)
  (condition-case nil
      (lsp)
    (error nil))

  (make-local-variable 'company-transformers)
  (push 'js-company-transformer company-transformers))

(add-hook 'js-mode-hook #'my-js-hook)

;; web-mode
(defun setup-web-mode ()
  "Setup the web mode."
  (web-mode)

  (local-set-key (kbd "<return>") 'newline-and-indent)

  (setq indent-tabs-mode nil)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-indent-style 4))

(add-to-list 'auto-mode-alist '("\\.html$" . setup-web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . setup-web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . setup-web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . setup-web-mode))

(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(setq css-indent-offset 2)

;; emmet
(dolist (hook '(sgml-mode-hook
                css-mode-hook
                web-mode-hook))
  (add-hook hook (lambda ()
                   (emmet-mode))))

(add-hook 'json-mode-hook
          (lambda ()
            (lsp-mode -1)
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(provide 'setup-web)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-web.el ends here
