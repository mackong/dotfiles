;;; setup-java.el --- setup for java development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for java development.

;;; Code:

(require 'lsp-java)

(setq lsp-java-workspace-dir (expand-file-name "~/.emacs.d/others/jdt/workspace")
      lsp-java-workspace-cache-dir (expand-file-name "~/.emacs.d/others/jdt/workspace/.cache")
      lsp-java-server-install-dir (expand-file-name "~/.emacs.d/others/jdt/server")
      lsp-java-import-maven-enabled nil)
(add-hook 'java-mode-hook 'lsp-java-enable)

(provide 'setup-java)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-java.el ends here
