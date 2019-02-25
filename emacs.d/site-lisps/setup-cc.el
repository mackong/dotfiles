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
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'innamespace 0)

  (doxymacs-mode)
  (doxymacs-font-lock)
  (hs-minor-mode 1)
  (add-to-list 'hs-special-modes-alist
               '(c-mode "[\n\t ]*{" "}" "/[*/]" nil nil))
  (local-set-key (kbd "M-+") 'hs-toggle-hiding))

(defun create-dot-cquery ()
  "Create a default .cquery in current project root."
  (interactive)
  (with-current-buffer (find-file (concat (projectile-project-root) ".cquery"))
    (insert "%clang
%c -std=gnu11
%cpp -std=gnu++14
-pthread")))

(defun my-cquery-cache-dir-in-project (proj-dir)
  "Return project relative cache directory (see cquery-cache-dir-function).

The name of the project-relative directory used for this is given by cquery-cache-dir."
  (expand-file-name cquery-cache-dir (projectile-root-bottom-up proj-dir)))

(defun setup-c/c++-mode ()
  "Setup for c/c++ mode"
  (c-set-style "linux")

  (setup-c-mode-common)

  (setq gdb-many-windows t)
  (setq gdb-use-separate-io-buffer t)

  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cquery-cache-dir-function #'my-cquery-cache-dir-in-project)
  (if (not (string-equal major-mode "glsl-mode"))
      (condition-case nil
          (progn
            (require 'cquery)

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

  (setup-c-mode-common)

  (condition-case nil
      (progn
        (require 'lsp-java)

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
