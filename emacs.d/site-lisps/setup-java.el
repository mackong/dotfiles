;;; setup-java.el --- setup for java development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for java development.

;;; Code:

(require 'lsp-java)

(defun set-java-workspace-folder (folder)
  "Set FOLDER as jdt workspace folder."
  (interactive (list (read-directory-name "workspace folder: " nil)))
  (setq lsp-java--workspace-folders (list folder)))

(defun setup-java-mode ()
  "Setup for java mode."
  (setq lsp-java-workspace-dir (expand-file-name "~/.jdt-workspace/")
        lsp-java-workspace-cache-dir (expand-file-name "~/.jdt-workspace/.cache/")
        lsp-java-server-install-dir "/opt/jdt"
        lsp-java-import-maven-enabled nil)
  (lsp-java-enable)

  (local-set-key (kbd "C-.") 'xref-find-definitions)
  (local-set-key (kbd "C-,") 'xref-pop-marker-stack)
  (local-set-key (kbd "C-c C-j") 'helm-imenu))

(add-hook 'java-mode-hook 'setup-java-mode)
(global-set-key (kbd "C-c j w f") 'set-java-workspace-folder)

(provide 'setup-java)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-java.el ends here
