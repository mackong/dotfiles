;;; setup-el-get.el --- setup various packages to install

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup various packages to install

;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun eval-url (url)
  "Fetch and eval the URL."
  (let ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun install-el-get ()
  "Install the el-get itself."
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(unless (require 'el-get nil t)
  (install-el-get))

(setq el-get-sources
      '((:name emacs-euler
               :description "Fetch project euler problem into file."
               :type github
               :pkgname "mackong/emacs-euler")
        (:name bongo
               :description "Play music with GNU Emacs"
               :type github
               :pkgname "dbrock/bongo"
               :build (("makeinfo" "-o" "bongo.info" "bongo.texi"))
               :info "bongo.info")
        (:name go-playground
               :description "GNU/Emacs mode that setup local Go playground for code snippets like play.golang.org or even better :)"
               :type github
               :pkgname "grafov/go-playground"
               :depends (go-test))
        (:name cdlatex-mode
               :description "Fast input methods for LaTeX environments and math"
               :type github
               :pkgname "cdominik/cdlatex")
        (:name company-lsp
               :description "Company completion backend for lsp-mode"
               :type github
               :pkgname "tigersoldier/company-lsp")
        (:name lsp-java
               :description "Java support for lsp-mode using the Eclipse JDT Language Server."
               :type github
               :pkgname "emacs-lsp/lsp-java"
               :depends (lsp-mode ht request hydra))
        (:name helm-rg
               :description "ripgrep is nice"
               :type github
               :pkgname "cosmicexplorer/helm-rg")
        (:name helm-xref
               :description "Helm interface for xref results"
               :type github
               :pkgname "brotzeit/helm-xref"
               :features helm-xref)
        (:name transient
               :description "Transient commands"
               :type github
               :load-path ("lisp")
               :pkgname "magit/transient")
        (:name dash-docs
               :description "A elisp library that exposes functionality to work with and search dash docsets."
               :type github
               :pkgname "dash-docs-el/dash-docs")
        (:name rfc-mode
               :description "An Emacs major mode to read and browse RFC documents."
               :type github
               :pkgname "galdor/rfc-mode"
               :features rfc-mode)
        (:name major-mode-hydra
               :description "Spacemacs-esque major mode leader key powered by Hydra"
               :type github
               :pkgname "jerrypnz/major-mode-hydra.el"
               :depends (hydra))
        (:name alect-theme
               :description "Configurable custom themes for GNU Emacs"
               :type github
               :pkgname "alezost/alect-themes"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name forge
               :description "Work with Git forges from the comfort of Magit"
               :type github
               :load-path "lisp/"
               :pkgname "magit/forge"
               :depends (ghub closql emacsql)
               :features forge)
        ))

(setq my-el-get-packages
      (append
       '(undo-tree
         irfc
         window-number
         multi-term
         restclient
         expand-region
         change-inner
         multiple-cursors
         exec-path-from-shell
         magit
         paredit
         multi-scratch
         visual-regexp
         dired-plus
         google-translate
         pdf-tools
         emojify
         avy

         helm-dash
         helm-projectile

         auctex
         plantuml-mode
         graphviz-dot-mode
         markdown-mode
         json-mode
         glsl-mode
         protobuf-mode
         doxymacs
         org-bullets
         org-jira

         flycheck
         lsp-ui
         ein

         web-mode
         emmet-mode
         js2-mode

         slime-company
         geiser
         yasnippet-snippets

         sphinx-doc
         pyvenv

         scala-mode
         sbt-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

(provide 'setup-el-get)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-el-get.el ends here
