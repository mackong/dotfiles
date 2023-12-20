;;; init.el --- setup for packages

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/dotfiles/emacs.d

;;; Commentary:

;; Setup for packages

;;; Code:

;; straight
(defvar bootstrap-version)
(setq straight-process-buffer " *straight-process*")
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
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME" "PYDEVD_DISABLE_FILE_VALIDATION" "JAVA_HOME" "GOPATH" "GOROOT" "RIPGREP_CONFIG_PATH" "DEBUGINFOD_URLS"))
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

;; compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error)

;; imenu
(setq imenu-max-item-length 'Unlimited)

;; eldoc
(setq eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message t
      eldoc-echo-area-prefer-doc-buffer t)

;; eglot
(setq eglot-autoshutdown t)
(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:inherit highlight)))))

;; treesit
(setq treesit-font-lock-level 4)

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
(use-package emacs
  :config
  (setq modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (bold)))
        modus-themes-org-blocks 'gray-background
        modus-themes-org-agenda '((header-block . (no-scale)))
        modus-vivendi-palette-overrides
        '((bg-space red)))
  (load-theme 'modus-operandi t))

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
  (setq undo-tree-auto-save-history nil)
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
(use-package visual-regexp-steroids)

;; posframe
(use-package posframe
  :config
  (setq posframe-mouse-banish '(10 . 10)))

;; go-translate
(use-package go-translate
  :config
  (setq gts-translate-list '(("en" "zh"))
        gts-default-translator (gts-translator
                                :picker (gts-prompt-picker)
                                :engines (list (gts-google-engine))
                                :render (gts-posframe-pop-render :forecolor "black" :backcolor "#f2eff3"))))

;; avy
(use-package avy
  :bind
  (("M-g g" . avy-goto-line)
   ("M-g c" . avy-goto-word-1)))

;; ace-window
(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-background nil))

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
        ivy-wrap t
        ivy-use-selectable-prompt t)
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-backward-delete-char))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-parse-remote-buffer nil))

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
        projectile-known-projects-file (expand-file-name "~/.emacs.d/others/projectile/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode))

;; counsel-projectile
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)
        counsel-projectile-find-file-matcher 'ivy--re-filter))

(use-package counsel-dash
  :config
  (setq counsel-dash-enable-debugging nil
        counsel-dash-docsets-path "~/.emacs.d/others/docset"
        counsel-dash-browser-func 'browse-url))

;; auctex
(defun setup-tex-mode ()
  "Setup for tex mode."
  (turn-on-reftex)
  (company-auctex-init)
  (local-set-key (kbd "TAB") 'TeX-complete-symbol))

(defun TeX-eaf-sync-view ()
  (eaf-open (TeX-active-master (TeX-output-extension))))

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
        TeX-save-query nil
        LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
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
(use-package markdown-mode)

;; json-mode
(use-package json-mode)

;; glsl-mode
(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.gs\\'" . glsl-mode)))

;; protobuf-mode
(use-package protobuf-mode)

;; rmsbolt
(use-package rmsbolt)

;; org-bullets
(use-package org-bullets)

;; org-babels
(use-package ob-go)
(use-package ob-restclient)
(use-package ob-ipython)

;; org-exports
(use-package ox-gfm)

;; denote
(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/Documents/Notes/")
        denote-known-keywords '()))

;; company
(use-package company
  :bind (("C-c y" . company-yasnippet)
         ("C-c i" . company-manual-begin)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :hook ((prog-mode org-mode LaTeX-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-show-numbers t
        company-idle-delay 3
        company-echo-delay 0
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-backends '(company-cmake company-capf company-files (company-dabbrev-code company-keywords) company-dabbrev)))

;; slime
(use-package slime
  :defer t
  :init
  (setq slime-complete-symbol*-fancy t
        inferior-lisp-program "/usr/bin/sbcl"
        slime-lisp-implementations '((sbcl ("sbcl")))))

;; slime-company
(use-package slime-company
  :after (slime company)
  :init
  (slime-setup '(slime-fancy slime-asdf slime-banner slime-company)))

;; geiser-chez
(use-package geiser-chez)

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

;; rust-mode
(use-package rust-mode)

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

;; major-mode-hydra
(use-package major-mode-hydra)

;; eaf
(use-package eaf
  :if (display-graphic-p)
  :straight nil
  :load-path "~/.emacs.d/others/packages/emacs-application-framework"
  :init
  (use-package s)
  (use-package epc :defer t)
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  :config
  (dolist (app '("browser" "image-viewer" "markdown-previewer" "mindmap" "pdf-viewer" "pyqterminal"))
    (add-to-list 'load-path (format "~/.emacs.d/others/packages/emacs-application-framework/app/%s" app))
    (require (intern (format "eaf-%s" app))))
  (setq eaf-webengine-default-zoom 1.25)
  (setq eaf-webengine-font-size 18)
  (setq eaf-webengine-font-family default-font)
  (setq eaf-webengine-fixed-font-size 18)
  (setq eaf-webengine-fixed-font-family default-font)
  (setq eaf-browser-enable-autofill t)
  (setq eaf-pyqterminal-font-family default-font)
  (setq eaf-pyqterminal-font-size 18)
  (setq eaf-mindmap-edit-mode t)
  (setq eaf-mindmap-save-path "~/Documents/Diagrams")
  (setq eaf-pdf-dark-exclude-image t)
  (setq eaf-python-command "/usr/bin/python")
  (setq eaf-config-location "~/.emacs.d/others/eaf/")
  (setq browse-url-browser-function 'eaf-open-browser)
  (with-eval-after-load 'browse-url
      (add-to-list 'browse-url-handlers
                   (cons "\\`file://.*\\.xhtml" 'eaf-open-browser)))
  (dolist (kb (list eaf-browser-keybinding eaf-mindmap-keybinding eaf-markdown-previewer-keybinding))
    (eaf-bind-key nil "M-o" kb)))

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
