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
(defun setup-c-mode-common (c-basic-offset)
  "Setup for cc-mode-common-hook."
  (c-toggle-hungry-state 1)
  (setq-local indent-tabs-mode nil)
  (setq-local c-basic-offset c-basic-offset)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'innamespace 0))

(defun setup-c/c++-mode ()
  "Setup for c/c++ mode"
  (c-set-style "linux")

  (setup-c-mode-common 8)

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

;;;;;;;;;;;;;;;;;;
;; csharp
;;;;;;;;;;;;;;;;;;
(defun setup-csharp-mode ()
  "Setup for csharp-mode."
  (setup-c-mode-common 4)

  (condition-case nil
      (lsp)
    (error nil)))

(add-hook 'csharp-mode-hook #'setup-csharp-mode)

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

  (condition-case nil
      (progn
        (setq lsp-go-hover-kind "FullDocumentation")
        (lsp))
    (error nil)))

(add-hook 'go-mode-hook 'setup-go-mode)

;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;
(defun setup-python-mode ()
  "Setup for python mode."
  (sphinx-doc-mode t)
  (define-key python-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (python-shell-send-buffer t)))

  (require 'lsp-pyright)
  (condition-case nil
      (lsp)
    (error nil)))

(add-hook 'python-mode-hook 'setup-python-mode)

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

              (setq-local indent-tabs-mode nil)
              (local-set-key (kbd "C-.") 'xref-find-definitions)
              (local-set-key (kbd "C-,") 'xref-pop-marker-stack))))

(provide 'setup-prog)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-prog.el ends here
