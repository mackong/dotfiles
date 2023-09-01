;;; setup-cc.el --- setup for development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/dotfiles/emacs.d

;;; Commentary:

;; Setup for development

;;; Code:

;;;;;;;;;;;;;;;;;;
;; c/c++
;;;;;;;;;;;;;;;;;;
(defun c++-ts-mode--indent-styles ()
  "Override the built-in linux indentation style with some additional rules"
  `(;; Indent the body of namespace definitions.
    ((parent-is "declaration_list") parent-bol 0)

    ,@(alist-get 'linux (c-ts-mode--indent-styles 'cpp))))

(defun setup-c/c++-mode ()
  "Setup for c/c++ mode"
  (setq-local indent-tabs-mode nil)
  (customize-set-variable 'c-ts-mode-indent-offset 8)
  (customize-set-variable 'c-ts-mode-indent-style
        (if (derived-mode-p 'c-ts-mode) 'linux #'c++-ts-mode--indent-styles))

  (if (and (projectile-project-p) (not (string-equal major-mode "glsl-mode")))
      (eglot-ensure)))

(add-hook 'c-ts-mode-hook 'setup-c/c++-mode)
(add-hook 'c++-ts-mode-hook 'setup-c/c++-mode)
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;;;;;;;;;;;;;;;;;;
;; golang
;;;;;;;;;;;;;;;;;;
(defun setup-go-mode ()
  "Setup for go mode."
  (setq-local tab-width 4)
  (whitespace-mode -1)

  ;; use goimports for go-fmt
  (setq gofmt-command "goimports")

  ;; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  (if (projectile-project-p)
      (eglot-ensure)))

(add-hook 'go-ts-mode-hook 'setup-go-mode)
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;
(defun setup-python-mode ()
  "Setup for python mode."
  (sphinx-doc-mode t)
  (define-key python-mode-map (kbd "C-c C-c")
              (lambda () (interactive) (python-shell-send-buffer t)))

  (if (projectile-project-p)
      (eglot-ensure)))

(add-hook 'python-ts-mode-hook 'setup-python-mode)
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(pyvenv-workon (car (seq-filter '(lambda (x) (equal "daily" (car (split-string x "-")))) (pyvenv-virtualenv-list))))

;;;;;;;;;;;;;;;;;;
;; lisp dialects
;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))

(dolist (hook '(lisp-mode-hook
                lisp-interaction-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook
                scheme-mode-hook
                geiser-repl-mode-hook))
  (add-hook hook
            (lambda ()
              (paredit-mode)
              (eldoc-mode)
              (show-paren-mode)

              (setq-local indent-tabs-mode nil))))

(provide 'setup-prog)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-prog.el ends here
