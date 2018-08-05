;;; setup-web.el --- setup for web development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for web development.
;; Include html, javascript, css, php.

;;; Code:

(defun setup-js3-mode ()
  "Setup the js3 mode."
  (linum-mode)
  (tern-mode)

  (setq js3-auto-indent-p t
        js3-curly-indent-offset 0
        js3-enter-indents-newline t
        js3-expr-indent-offset 2
        js3-indent-on-enter-key t
        js3-lazy-commas t
        js3-lazy-dots t
        js3-lazy-operators t
        js3-paren-indent-offset 2
        js3-square-indent-offset 4)

  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-tern)))

(add-hook 'js3-mode-hook 'setup-js3-mode)

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
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(provide 'setup-web)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-web.el ends here
