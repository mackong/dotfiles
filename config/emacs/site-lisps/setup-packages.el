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
(push 'eldoc straight-built-in-pseudo-packages)

;; eglot
(setq eglot-autoshutdown t
      eglot-extend-to-xref t)
(push 'eglot straight-built-in-pseudo-packages)

;; eglot-java
(defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
  "Custom options that will be merged with any default settings."
  `(:bundles [,(full-emacs-dir "share/lsp/java/com.microsoft.java.debug.plugin.jar")]
    :settings (:java (:format (:settings (:url ,(concat "file://" (full-emacs-dir "share/lsp/java/codestyle.xml"))) :enabled t)))))

(use-package eglot-java
  :defer t
  :config
  (setq eglot-java-eclipse-jdt-args
        `(
          "-Xmx1G"
          "-XX:+UseG1GC"
          "-XX:+UseStringDeduplication"
          "--add-modules=ALL-SYSTEM"
          "--add-opens" "java.base/java.util=ALL-UNNAMED"
          "--add-opens" "java.base/java.lang=ALL-UNNAMED"
          ,(concat "-javaagent:" (full-emacs-dir "share/lsp/java/lombok.jar"))))
  (setq eglot-java-eclipse-jdt-cache-directory (full-emacs-dir "cache/eglot-java-eclipse-jdt-cache"))
  (setq eglot-java-server-install-dir (full-emacs-dir "share/lsp/java/eclipse.jdt.ls"))
  (setq eglot-java-junit-platform-console-standalone-jar (full-emacs-dir "share/lsp/java/junit-platform-console-standalone.jar"))
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts))

;; dape
(use-package dape
  :defer t
  :config
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-inlay-hints t)
  (setq dape-cwd-function 'projectile-project-root))

;; treesit
(setq treesit-font-lock-level 4)
(setq treesit-language-source-alist
   '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (templ "https://github.com/vrischmann/tree-sitter-templ")))

;; cmake-mode
(if (eq system-type 'darwin)
    (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/cmake"))
(require 'cmake-mode nil 'noerror)

;; maxima
(if (eq system-type 'darwin)
    (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/maxima"))
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "Large")

;; theme
(use-package emacs
  :config
  (load-theme 'modus-operandi-deuteranopia t))

;; nerd-icons
(use-package nerd-icons)

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
  :load-path (lambda () (full-emacs-dir "share/packages/multi-scratch")))

;; change-inner
(use-package change-inner
  :bind (("C-=" . er/expand-region)))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c m e" . mc/edit-lines)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m n" . mc/insert-numbers))
  :config
  (setq mc/list-file (full-emacs-dir "cache/.mc-lists.el")))

;; transient
(use-package transient
  :config
  (setq transient-levels-file (full-emacs-dir "cache/transient/levels.el")
        transient-values-file (full-emacs-dir "cache/transient/values.el")
        transient-history-file (full-emacs-dir "cache/transient/history.el")))

;; magit
(use-package magit)

;; paredit
(use-package paredit)

;; visual-regexp
(use-package visual-regexp-steroids)

;; gt
(use-package gt
  :config
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-chatgpt-host (concat "https://" (get-openai-host))
        gt-chatgpt-path (get-openai-endpoint)
        gt-chatgpt-model (or (getenv "GT_CHATGPT_MODEL") (getenv "OPENAI_MODEL"))
        gt-default-translator (gt-translator
                               :taker (list (gt-taker :prompt t))
                               :engines (list (gt-chatgpt-engine :stream t)
                                              (gt-google-engine))
                               :render (gt-buffer-render))))

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
  :bind (("C-s" . swiper)
         ("C-*" . swiper-thing-at-point))
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
    (setq amx-save-file (full-emacs-dir "cache/amx-items"))))

;; projectile
(straight-use-package 'project)
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-cache-file (full-emacs-dir "cache/projectile/projectile.cache")
        projectile-known-projects-file (full-emacs-dir "cache/projectile/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode))

;; counsel-projectile
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)
        counsel-projectile-find-file-matcher 'ivy--re-filter))

;; plantuml
(use-package plantuml-mode
  :config
  (setq plantuml-jar-path (full-emacs-dir "share/plantuml/plantuml.jar")
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

;; org-babels
(use-package ob-go)
(use-package ob-restclient)
(use-package ob-ipython)  ;; TODO: fix daemon=True

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

;; geiser-guile
(use-package geiser-guile
  :defer t
  :init
  (add-hook 'geiser-repl-mode-hook
          #'(lambda () (define-key paredit-mode-map (kbd "C-j") 'geiser-repl-maybe-send))))

;; yasnippet-snippets
(use-package yasnippet-snippets
  :defer t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (setq yas-prompt-functions '(yas-dropdown-prompt)))

;; sphinx-doc
(use-package sphinx-doc)

;; pet
(use-package pet)

;; go-mode
(use-package go-mode)

;; templ
(use-package templ-ts-mode)

;; rust-mode
(use-package rust-mode)

;; bongo
(use-package bongo
  :config
  (setq bongo-default-directory "~/Music"
        bongo-insert-whole-directory-trees t))

;; rfc-mode
(use-package irfc
  :straight nil
  :load-path (lambda () (full-emacs-dir "share/packages/irfc"))
  :init
  (setq irfc-directory "~/Documents/RFC"
        irfc-assoc-mode t))

;; major-mode-hydra
(use-package major-mode-hydra)

;; aider
(use-package aider
  :config
  (setq aider-args `("--model" ,(get-aider-model))))

;; gptel
(use-package gptel
  :config
  (require 'gptel-integrations)
  (setq gptel-default-mode 'org-mode
        gptel-model 'deepseek-v3.1
        gptel-backend (gptel-make-openai "DeepSeek"
                        :stream t
                        :models '((deepseek-v3.1
                                   :description "DeepSeek V3.1"
                                   :capabilities (media tool-use json url)
                                   :context-window 128
                                   :input-cost 0.004
                                   :output-cost 0.012))
                        :host (get-openai-host)
                        :endpoint (get-openai-endpoint)
                        :key (get-openai-apikey)))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; mcp
(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  :after gptel
  :config (require 'mcp-hub))

;; helpful
(use-package helpful
  :bind (("C-h k" . helpful-key))
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

;; indent-bars
(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars" :files ("*.el"))
  :custom
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth nil)
  (indent-bars-width-frac 0.15)
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t))

;; centaur-tabs
(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-height 32
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'over
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"
        centaur-tabs-show-navigation-buttons nil
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-set-close-button nil
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-cycle-scope 'tabs)
  :config
  (centaur-tabs-mode t)
  (dotimes (n 10) (global-set-key (kbd (format "M-%d" n)) 'centaur-tabs-select-visible-tab)))

;; prettier-js
(use-package prettier-js
  :config
  (setq prettier-js-use-modules-bin t))

(provide 'setup-packages)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-packages.el ends here
