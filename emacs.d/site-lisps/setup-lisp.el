;;; setup-lisp.el --- setup for various Lisp dialects development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for various Lisp dialetcts(Common Lisp, elisp) development.

;;; Code:

(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))

;; autopair, paredit, highlight-parentheses
(dolist (hook '(lisp-mode-hook
                lisp-interaction-mode-hook
                emacs-lisp-mode-hook
                slime-repl-mode-hook
                scheme-mode-hook))
  (add-hook hook
            (lambda ()
              (paredit-mode)
              (eldoc-mode)
              (show-paren-mode)

              (setq-local indent-tabs-mode nil)
              (local-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
              (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
              (local-set-key (kbd "C-.") 'xref-find-definitions)
              (local-set-key (kbd "C-,") 'xref-pop-marker-stack))))

(provide 'setup-lisp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-lisp.el ends here
