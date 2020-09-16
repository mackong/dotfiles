;;; init.el --- setup for packages

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for packages

;;; Code:

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-check-for-modification '(check-on-save))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME" "JAVA_HOME" "GOPATH" "GOROOT"))
  (exec-path-from-shell-initialize))

;; theme
(use-package all-the-icons)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dark+ t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; dired-plus
(use-package dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; window-number
(use-package window-number
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1)
  (setq mode-line-modes
        (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
          (list (propertize "%[" 'help-echo recursive-edit-help-echo)
                "("
                `(:propertize ("" mode-name)
                              help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                              mouse-face mode-line-highlight
                              local-map ,mode-line-major-mode-keymap)
                '("" mode-line-process)
                ")"
                (propertize "%]" 'help-echo recursive-edit-help-echo)
                " "
                "["
                `(:eval (number-to-string (window-number)))
                "]"
                ))))

;; multi-scratch
(use-package multi-scratch
  :straight nil
  :load-path "~/.emacs.d/others/packages/multi-scratch")

;; multi-term
(defun term-toggle-mode ()
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(defun setup-term ()
  (setq-local global-hl-line-mode nil)
  (yas-minor-mode -1)
  (compilation-shell-minor-mode t)

  (define-key term-mode-map (kbd "C-c C-j") 'term-toggle-mode)
  (define-key term-raw-map (kbd "C-c C-j") 'term-toggle-mode))

(use-package multi-term
  :config
  (add-hook 'term-mode-hook #'setup-term))

;; change-inner
(use-package change-inner
  :bind (("C-=" . er/expand-region)))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c m e" . mc/edit-lines)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m n" . mc/insert-numbers))
  :config
  (setq mc/list-file (expand-file-name "~/.emacs.d/others/.mc-lists.el")))

;; magit
(use-package magit
  :defer t)

;; paredit
(use-package paredit)

;; visual-regexp
(use-package visual-regexp)

;; google-translate
(use-package google-translate
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "zh-CN")
          ("zh-CN" . "en"))))

;; pdf-tools
(defun setup-pdf-tools ()
  (pdf-view-midnight-minor-mode)
  (setq pdf-view-display-size 'fit-page
        pdf-view-resize-factor 1.1))

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :no-query)
  (add-hook 'pdf-tools-enabled-hook #'setup-pdf-tools))

;; avy
(use-package avy
  :bind (("M-g g" . avy-goto-line)
         ("M-g c" . avy-goto-subword-1)
         ("C-*" . isearch-forward-symbol-at-point)))

;; symbol-overlay
(use-package symbol-overlay
  :bind (("M-*" . symbol-overlay-put)))

;; helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini))
  :preface (require 'helm-config)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-buffer-max-length nil))

;; projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'helm
        projectile-cache-file (expand-file-name "~/.emacs.d/others/projectile/projectile.cache")
        projectile-known-projects-file (expand-file-name "~/.emacs.d/el-get/projectile/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode)
  (setq projectile-globally-ignored-directories (append '("bin" "pkg" "vendor"  ;; for golang workspaces
                                                          "project" "target" ".settings"  ;; for maven project
                                                          ".metals" ".bloop"    ;; for sbt project
                                                          ) projectile-globally-ignored-directories)))

;; helm-dash
(use-package helm-dash)

;; helm-projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; helm-rg
(use-package helm-rg)

;; helm-xref
(use-package helm-xref
  :config
  (setq helm-xref-candidata-formatting-function 'helm-xref-format-candidate-long)
  (setq-default xref-prompt-for-identifier nil))

;; auctex
(defun setup-tex-mode ()
  "Setup for tex mode."
  (turn-on-reftex)
  (company-auctex-init)
  (local-set-key (kbd "TAB") 'TeX-complete-symbol))

(use-package auctex
  :defer t
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-auto-untabify t
        TeX-engine 'xetex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-global-PDF-mode t
        TeX-save-query nil)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook 'setup-tex-mode))

(use-package company-auctex)

;; plantuml
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar))

;; graphviz-dot-mode
(use-package graphviz-dot-mode)

;; markdown
(use-package markdown-mode)

;; json-mode
(use-package json-mode)

;; glsl-mode
(use-package glsl-mode)

;; protobuf-mode
(use-package protobuf-mode)

;; org-bullets
(use-package org-bullets
  :defer t)

;; org-jira
(use-package org-jira
  :defer t)

;; ob-go
(use-package ob-go
  :defer t)

;; ob-restclient
(use-package ob-restclient
  :defer t)

;; flycheck
(use-package flycheck
  :defer t)

;; company
(use-package company
  :bind (("C-c y" . company-yasnippet)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook ((prog-mode org-mode LaTeX-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-numbers t
        company-idle-delay .1
        company-echo-delay 0
        company-backends '(company-nxml company-css company-semantic company-cmake company-capf company-files
                                        (company-dabbrev-code company-keywords) company-dabbrev)))

;; treemacs
(use-package treemacs
  :config
  (setq treemacs-position 'right
        treemacs-persist-file (expand-file-name "~/.emacs.d/others/treemacs-persist")))

;; lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-prefer-flymake nil
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil
        company-lsp-cache-candidates 'auto
        lsp-enable-links nil
        lsp-enable-folding nil
        lsp-server-install-dir (expand-file-name "~/.emacs.d/others/lsp")
        lsp-session-file (expand-file-name "~/.emacs.d/others/.lsp-session-v1")
        lsp-treemacs-deps-position-params `((side . left) (slot . 1) (window-width . ,treemacs-width))
        lsp-treemacs-symbols-position-params `((side . left) (slot . 2) (window-width . ,treemacs-width)))
  :bind (:map lsp-mode-map
              ("C-." . xref-find-definitions)
              ("C-," . xref-pop-marker-stack)))

;; lsp-ui
(use-package lsp-ui)

;; lsp-java
(use-package lsp-java
  :init
  (setq lsp-java-workspace-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/workspace")
        lsp-java-workspace-cache-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/workspace/.cache")
        lsp-java-jdt-download-url "https://mirrors.tuna.tsinghua.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz"
        lsp-java-server-install-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/server")
        lsp-java-import-maven-enabled t
        lsp-java-import-gradle-enabled nil))

;; lsp-metals
(use-package lsp-metals
  :config
  (add-hook 'scala-mode-hook #'lsp))

;; lsp-pyright
(use-package lsp-pyright
  :config
  (setq lsp-pyright-log-level "error"))

;; dap-mode
(use-package dap-mode
  :config
  (setq dap-breakpoints-file (expand-file-name "~/.emacs.d/others/.dap-breakpoints"))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; company-lsp
(use-package company-lsp)

;; slime-company
(use-package slime-company)

;; slime
(use-package slime
  :after (slime-company)
  :config
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-company))
  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        inferior-lisp-program "/usr/bin/sbcl"
        slime-lisp-implementations '((sbcl ("sbcl")))))

;; geiser
(use-package geiser
  :init
  (setq geiser-active-implementations '(chez)))

;; yasnippet-snippets
(use-package yasnippet-snippets
  :defer t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-dropdown-prompt)))

;; sphinx-doc
(use-package sphinx-doc)

;; pyvenv
(use-package pyvenv)

;; go-mode
(use-package go-mode)

;; scala-mode
(use-package scala-mode)

;; bongo
(use-package bongo
  :config
  (setq bongo-default-directory (expand-file-name "~/Music")
        bongo-insert-whole-directory-trees t))

;; rfc-mode
(use-package rfc-mode
  :config
  (setq rfc-mode-directory (expand-file-name "~/Documents/RFC/")))
(use-package irfc
  :straight nil
  :load-path "~/.emacs.d/others/packages/irfc/"
  :hook ((rfc-mode) . irfc-mode)
  :config
  (setq irfc-directory "~/Documents/RFC"
        irfc-assoc-mode t))

;; major-mode-hydra
(use-package major-mode-hydra)

;; ein
(defun setup-ein ()
  (set-face-attribute 'ein:cell-input-area nil :background "#121212")
  (set-face-attribute 'ein:cell-output-area nil :background "#121212")
  (whitespace-mode -1))

(use-package ein
  :config
  (setq ein:jupyter-default-notebook-directory (expand-file-name "~/Codes/python/daily/notebooks"))
  (add-hook 'ein:notebook-mode-hook #'setup-ein))

(provide 'setup-packages)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-packages.el ends here
