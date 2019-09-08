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
        (:name symbol-overlay
               :description "Highlight symbols with keymap-enabled overlays"
               :type github
               :pkgname "wolray/symbol-overlay")
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
        (:name dwim-compile
               :description "context-aware compile command"
               :type http
               :url "https://raw.githubusercontent.com/cinsk/emacs-scripts/master/src/dwim-compile.el")
        (:name cdlatex-mode
               :description "Fast input methods for LaTeX environments and math"
               :type github
               :pkgname "cdominik/cdlatex")
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
               :pkgname "emacs-lsp/lsp-java"
               :depends (lsp-mode ht request))
        (:name lsp-python-ms
               :description "emacs lsp-mode client for Microsoft's python language server"
               :type github
               :pkgname "emacs-lsp/lsp-python-ms")
        (:name helm-rg
               :description "ripgrep is nice"
               :type github
               :pkgname "cosmicexplorer/helm-rg")
        (:name transient
               :description "Transient commands"
               :type github
               :load-path ("lisp")
               :pkgname "magit/transient")
        (:name cyberpunk-theme
               :description "Cyberpunk theme for emacs built-in color theme support"
               :type github
               :pkgname "mackong/cyberpunk-theme.el"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        (:name dash-docs
               :description "A elisp library that exposes functionality to work with and search dash docsets."
               :type github
               :pkgname "dash-docs-el/dash-docs")
        (:name rfc-mode
               :description "An Emacs major mode to read and browse RFC documents."
               :type github
               :pkgname "galdor/rfc-mode")
        (:name emacs-zmq
               :description "Emacs bindings to ØMQ"
               :type github
               :pkgname "dzop/emacs-zmq"
               :build (("make")))
        (:name emacs-jupyter
               :description "An interface to communicate with Jupyter kernels."
               :type github
               :pkgname "dzop/emacs-jupyter"
               :depends (simple-httpd websocket emacs-zmq))
        ))

(setq my-el-get-packages
      (append
       '(undo-tree
         irfc
         window-number
         multi-term
         avy
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
         smart-mode-line

         helm-dash
         helm-projectile

         plantuml-mode
         gnuplot-mode
         graphviz-dot-mode
         markdown-mode
         json-mode
         glsl-mode
         protobuf-mode
         doxymacs

         yasnippet-snippets
         flycheck
         lsp-ui

         company-auctex

         web-mode
         emmet-mode
         js2-mode

         slime-company

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
