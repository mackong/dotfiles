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
  (c-set-offset 'innamespace 0)

  (doxymacs-mode)
  (doxymacs-font-lock))

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

(defun java-inside-lambda-p ()
  "Returns true if point is the first statement inside of a lambda"
  (save-excursion
    (c-beginning-of-statement-1)
    (let ((start (point)))
      (forward-line -1)
      (if (search-forward " -> {" start t) t nil))))

(defun java-end-of-lambda-p ()
  (save-excursion
    (move-beginning-of-line nil)
    (let ((bound (line-end-position)))
      (if (search-forward ");" bound t) t nil))))

(defun java-statement-block-intro (arg)
  (if (and (c-at-statement-start-p) (java-inside-lambda-p))
      0
    '+))

(defun java-block-close (arg)
  (if (java-end-of-lambda-p)
      '-
    0))

(defun setup-java-mode ()
  "Setup for java mode"
  (c-set-style "java")
  (c-set-offset 'statement-block-intro 'java-statement-block-intro)
  (c-set-offset 'block-close 'java-block-close)
  (c-set-offset 'arglist-intro '+)

  (setup-c-mode-common)

  (setq-local c-basic-offset 4)

  (condition-case nil
      (progn
        (require 'lsp-java)

        (setq lsp-java-format-on-type-enabled nil
              lsp-java-format-enabled nil
              lsp-java-signature-help-enabled t)
        (lsp))
    (error nil)))

(setq lsp-java-workspace-dir (expand-file-name "~/.emacs.d/others/jdt/workspace")
      lsp-java-workspace-cache-dir (expand-file-name "~/.emacs.d/others/jdt/workspace/.cache")
      lsp-java-server-install-dir (expand-file-name "~/.emacs.d/others/jdt/server")
      lsp-java-import-maven-enabled t
      lsp-java-import-gradle-enabled nil)
(add-hook 'java-mode-hook 'setup-java-mode)

(provide 'setup-cc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-cc.el ends here
