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

;; linum-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; woman
(setq woman-fill-frame t
      woman-use-own-frame nil
      woman-cache-level 3)

;; whitespace mode
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-style '(face tabs empty trailing))

;; nxml
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; cmake-mode
(require 'cmake-mode nil 'noerror)

;; maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "Large")

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

;; modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; dired
(require 'dired-x)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 0)))
(setq-default dired-omit-files-p t)
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-dwim-target t
      dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; dired-plus
(use-package dired+
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; multi-scratch
(use-package multi-scratch
  :straight nil
  :load-path "~/.emacs.d/others/packages/multi-scratch")

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

;; transient
(use-package transient
  :config
  (setq transient-levels-file "~/.emacs.d/others/transient/levels.el"
        transient-values-file "~/.emacs.d/others/transient/values.el"
        transient-history-file "~/.emacs.d/others/transient/history.el"))

;; magit
(use-package magit)

;; paredit
(use-package paredit)

;; visual-regexp
(use-package visual-regexp)

;; go-translate
(use-package go-translate
  :config
  (setq go-translate-base-url "https://translate.google.cn"
        go-translate-token-current (cons 430675 2721866130)
        go-translate-local-language "zh-CN"
        go-translate-buffer-follow-p t
        go-translate-inputs-function #'go-translate-inputs-current-or-prompt))

;; treemacs
(use-package treemacs
  :config
  (setq treemacs-position 'right
        treemacs-persist-file (expand-file-name "~/.emacs.d/others/treemacs-persist")))

;; ace-window
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers)))

;; avy
(use-package avy
  :bind (("M-g l" . avy-goto-line)
         ("M-g c" . avy-goto-char)))

;; symbol-overlay
(use-package symbol-overlay
  :bind (("M-*" . symbol-overlay-put)))

;; ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :bind (("C-*" . swiper-thing-at-point))
  :config
  (setq ivy-use-virtual-buffers 'recentf
        ivy-fixed-height-minibuffer nil
        ivy-height 13
        ivy-wrap t)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-delete-char))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-parse-remote-buffer nil))

(use-package ivy-posframe
  :after ivy
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-posframe-border-width 1
        ivy-posframe-parameters '((left-fringe . 4) (right-fringe . 4))
        ivy-posframe-min-height 19
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (use-package amx
    :config
    (setq amx-save-file "~/.emacs.d/others/amx-items")))

;; projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-cache-file (expand-file-name "~/.emacs.d/others/projectile/projectile.cache")
        projectile-known-projects-file (expand-file-name "~/.emacs.d/el-get/projectile/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode)
  (setq projectile-globally-ignored-directories (append '("bin" "pkg" "vendor"  ;; for golang workspaces
                                                          "project" "target" ".settings"  ;; for maven project
                                                          ".metals" ".bloop"    ;; for sbt project
                                                          ) projectile-globally-ignored-directories)))

;; counsel-projectile
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)
        counsel-projectile-find-file-matcher 'ivy--re-filter))

;; auctex
(defun setup-tex-mode ()
  "Setup for tex mode."
  (turn-on-reftex)
  (company-auctex-init)
  (local-set-key (kbd "TAB") 'TeX-complete-symbol))

(defun TeX-eaf-sync-view ()
  (eaf-open (concat file "." (TeX-output-extension))))

(use-package auctex
  :defer t
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-auto-untabify t
        TeX-engine 'xetex
        TeX-view-program-list '(("EAF" TeX-eaf-sync-view))
        TeX-view-program-selection '((output-pdf "EAF"))
        TeX-source-correlate-start-server t
        TeX-global-PDF-mode t
        TeX-save-query nil)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook 'setup-tex-mode))

(use-package company-auctex)

;; plantuml
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path "~/.emacs.d/others/plantuml.jar"
        plantuml-default-exec-mode 'jar))

;; graphviz-dot-mode
(use-package graphviz-dot-mode)

;; markdown
(use-package markdown-mode
  :mode "\\.mmd\\'")

;; json-mode
(use-package json-mode)

;; glsl-mode
(use-package glsl-mode)

;; protobuf-mode
(use-package protobuf-mode)

;; org-bullets
(use-package org-bullets)

;; org-jira
(use-package org-jira
  :defer t)

;; org-mime
(use-package org-mime)

;; ob-go
(use-package ob-go)

;; ob-restclient
(use-package ob-restclient)

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
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-backends '(company-nxml company-css company-semantic company-cmake company-capf company-files
                                        (company-dabbrev-code company-keywords) company-dabbrev)))

;; lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil
        lsp-enable-links nil
        lsp-enable-folding nil
        lsp-enable-snippet nil
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-diagnostics-provider :none
        lsp-modeline-code-actions-enable nil
        lsp-server-install-dir (expand-file-name "~/.emacs.d/others/lsp")
        lsp-session-file (expand-file-name "~/.emacs.d/others/.lsp-session-v1")
        lsp-treemacs-deps-position-params `((side . left) (slot . 1) (window-width . ,treemacs-width))
        lsp-treemacs-symbols-position-params `((side . left) (slot . 2) (window-width . ,treemacs-width)))
  :bind (:map lsp-mode-map
              ("C-." . xref-find-definitions)
              ("C-," . xref-pop-marker-stack)))

;; lsp-java
(use-package lsp-java
  :init
  (setq lsp-java-workspace-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/workspace")
        lsp-java-workspace-cache-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/workspace/.cache")
        lsp-java-jdt-download-url "https://mirrors.tuna.tsinghua.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz"
        lsp-java-java-path "/usr/lib/jvm/java-14-openjdk/bin/java"
        lsp-java-server-install-dir (expand-file-name "~/.emacs.d/others/lsp/jdt/server")
        dap-java-test-runner (expand-file-name "~/.emacs.d/others/lsp/jdt/test-runner/junit-platform-console-standalone.jar")
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

;; slime
(use-package slime
  :init
  (setq slime-complete-symbol*-fancy t
        inferior-lisp-program "/usr/bin/sbcl"
        slime-lisp-implementations '((sbcl ("sbcl")))))

;; slime-company
(use-package slime-company
  :after (slime company)
  :init
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-company)))

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

;; go-dlv
(use-package go-dlv)

;; scala-mode
(use-package scala-mode)

;; bongo
(use-package bongo
  :config
  (setq bongo-default-directory (expand-file-name "~/Music")
        bongo-insert-whole-directory-trees t))

;; rfc-mode
(use-package irfc
  :straight nil
  :load-path "~/.emacs.d/others/packages/irfc/"
  :init
  (setq irfc-directory "~/Documents/RFC"
        irfc-assoc-mode t))

;; hydra-posframe
(use-package hydra-posframe
  :straight nil
  :load-path "~/.emacs.d/others/packages/hydra-posframe"
  :hook (after-init . hydra-posframe-mode)
  :custom
  (hydra-posframe-parameters '((left-fringe . 4) (right-fringe . 4) (top-fringe . 4) (bottom-fringe . 4) (min-height . 13) (max-height . 30))))

;; major-mode-hydra
(use-package major-mode-hydra)

;; eaf
(use-package eaf
  :straight nil
  :load-path "~/.emacs.d/others/packages/emacs-application-framework"
  :init
  (use-package s)
  (use-package epc :defer t)
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  :config
  (eaf-setq eaf-browser-default-zoom "1.5")
  (eaf-setq eaf-browser-font-family "Fira Code")
  (eaf-setq eaf-browser-enable-autofill "true")
  (eaf-setq eaf-terminal-font-size "12")
  (eaf-setq eaf-terminal-font-family "Fira Code")
  (eaf-setq eaf-mindmap-edit-mode "true")
  (eaf-setq eaf-mindmap-save-path "~/Documents/Diagrams")
  (eaf-setq eaf-jupyter-font-size "12")
  (eaf-setq eaf-jupyter-font-family "Fira Code")
  (eaf-setq eaf-pdf-dark-exclude-image "true")
  (setq eaf-python-command "~/.conda/envs/daily/bin/python")
  (setq eaf-config-location "~/.emacs.d/others/eaf/")
  (setq eaf-proxy-type "http"
        eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "12345")
  (setq browse-url-browser-function 'eaf-open-browser)
  (eaf-bind-key add_sub_node "<tab>" eaf-mindmap-keybinding)
  (eaf-bind-key add_brother_node "<return>" eaf-mindmap-keybinding)
  (dolist (kb (list eaf-browser-keybinding eaf-mindmap-keybinding))
    (eaf-bind-key nil "M-o" kb)))

;; awesome-tab
(use-package awesome-tab
  :straight nil
  :load-path "~/.emacs.d/others/packages/awesome-tab"
  :hook (after-init . awesome-tab-mode)
  :bind (("M-1" . awesome-tab-select-visible-tab)
         ("M-2" . awesome-tab-select-visible-tab)
         ("M-3" . awesome-tab-select-visible-tab)
         ("M-4" . awesome-tab-select-visible-tab)
         ("M-5" . awesome-tab-select-visible-tab)
         ("M-6" . awesome-tab-select-visible-tab)
         ("M-7" . awesome-tab-select-visible-tab)
         ("M-8" . awesome-tab-select-visible-tab)
         ("M-9" . awesome-tab-select-visible-tab)
         ("M-0" . awesome-tab-select-visible-tab))
  :init
  (setq awesome-tab-label-fixed-length 14))

;; helpful
(use-package helpful
  :bind (("C-h k" . helpful-key))
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(provide 'setup-packages)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-packages.el ends here
