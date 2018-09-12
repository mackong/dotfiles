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
      '((:name dpaste
               :description "Emacs mode to post to dpaste.com"
               :type github
               :pkgname "mackong/dpaste.el")
        (:name emacs-euler
               :description "Fetch project euler problem into file."
               :type github
               :pkgname "mackong/emacs-euler")
        (:name emacs-angular2-helper
               :description "Helper functions to make angular2 development more easily."
               :type github
               :pkgname "mackong/emacs-angular2-helper")
        (:name bongo
               :description "Play music with GNU Emacs"
               :type github
               :pkgname "dbrock/bongo"
               :build (("makeinfo" "-o" "bongo.info" "bongo.texi"))
               :info "bongo.info")
        (:name go-dlv
               :description "Go Delve - Debug Go programs interactively with the GUD."
               :type github
               :pkgname "benma/go-dlv.el")
        (:name go-playground
               :description "GNU/Emacs mode that setup local Go playground for code snippets like play.golang.org or even better :)"
               :type github
               :pkgname "grafov/go-playground"
               :depends (go-test))
        (:name dwim-compile
               :description "context-aware compile command"
               :type http
               :url "https://raw.githubusercontent.com/cinsk/emacs-scripts/master/src/dwim-compile.el")
        (:name cdlatex-mode
               :description "Fast input methods for LaTeX environments and math"
               :type github
               :pkgname "cdominik/cdlatex")
        (:name org-bullets
               :description "Show org-mode bullets as UTF-8 characters."
               :type github
               :pkgname "sabof/org-bullets"
               :features org-bullets)
	(:name lsp-mode
	       :description "Emacs client/library for the Language Server Protocol"
	       :type github
	       :pkgname "emacs-lsp/lsp-mode")
        (:name lsp-ui
	       :description "This contains all the higher level UI modules of lsp-mode"
	       :type github
	       :pkgname "emacs-lsp/lsp-ui"
               :depends (flycheck))
        (:name company-lsp
	       :description "Company completion backend for lsp-mode"
	       :type github
	       :pkgname "tigersoldier/company-lsp")
	(:name emacs-cquery
	       :description "Emacs client for cquery, a low-latency language server supporting multi-million line C++ code-bases"
	       :type github
	       :pkgname "cquery-project/emacs-cquery")
        (:name lsp-java
               :description "Java support for lsp-mode using the Eclipse JDT Language Server."
               :type github
               :pkgname "emacs-lsp/lsp-java")
        ))

(setq my-el-get-packages
      (append
       '(undo-tree
         irfc
         window-number
         multi-term
         autopair
         avy
         json-reformat
         restclient
         rainbow-identifiers
         rainbow-delimiters
         expand-region
         multiple-cursors
         sr-speedbar
         highlight-symbol
         exec-path-from-shell
         magit
         paredit
         multi-scratch
         smooth-scrolling
         editorconfig
         dired-plus
         monokai-theme
         ob-ipython

         helm-dash
         helm-projectile

         gnuplot-mode
         graphviz-dot-mode
         adoc-mode
         markdown-mode
         json-mode

         yasnippet-snippets

         company-auctex

         js3-mode
         nodejs-repl
         web-mode
         jinja2-mode
         emmet-mode
         company-tern

         go-eldoc
         go-company

         slime-company

         sphinx-doc
         pyvenv

         glsl-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

(if (not (package-installed-p 'cl-generic))
    (package-install cl-generic))

(provide 'setup-el-get)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-el-get.el ends here
