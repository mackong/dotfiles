;;; setup-tex.el --- setup for latex.

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for latex.

;;; Code:

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(defun setup-tex-mode ()
  "Setup for tex mode."
  (turn-on-reftex)
  (turn-on-cdlatex)
  (company-auctex-init)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-auto-untabify t)
  (setq TeX-engine 'xetex)
  (setq TeX-view-program-list '(("Evince" "evince %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-global-PDF-mode t)
  (setq TeX-save-query nil)
  (local-set-key (kbd "TAB") 'TeX-complete-symbol))

(add-hook 'latex-mode-hook 'setup-tex-mode)
(add-hook 'LaTeX-mode-hook 'setup-tex-mode)

(provide 'setup-tex)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-tex.el ends here
