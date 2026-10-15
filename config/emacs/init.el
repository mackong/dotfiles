;; -*- lexical-binding: t; -*-

;;; init.el --- startup file for emacs

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/dotfiles/emacs.d

;;; Commentary:

;; Startup file for Emacs.

;;; Code:

(defun full-emacs-dir (name)
  (expand-file-name (concat user-emacs-directory name)))

(defun get-openai-host ()
  (url-host (url-generic-parse-url (getenv "OPENAI_BASE_URL"))))

(defun get-openai-endpoint ()
  (concat (url-filename (url-generic-parse-url (getenv "OPENAI_BASE_URL"))) "/chat/completions"))

(defun get-openai-apikey ()
  (getenv "OPENAI_API_KEY"))

(setq frame-title-format
      '("emacs@" (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))

(fringe-mode '(nil . 0))
(delete-selection-mode 1)
(pixel-scroll-precision-mode 1)

(setq mac-command-modifier 'meta)

(setq-default cursor-type 'box)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-default-init t)

(setq user-full-name "mackong")
(setq user-mail-address "mackonghp@gmail.com")

(setq package-user-dir (full-emacs-dir "cache/elpa"))
(setq auto-save-list-file-prefix (full-emacs-dir "cache/auto-save-list/.saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq recentf-save-file (full-emacs-dir "cache/recentf"))
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq eshell-directory-name (full-emacs-dir "cache/eshell/"))
(setq bookmark-save-flag nil)
(setq bookmark-default-file (full-emacs-dir "cache/bookmarks"))
(setq tramp-ssh-controlmaster-options "")
(setq tramp-persistency-file-name (full-emacs-dir "cache/tramp"))
(startup-redirect-eln-cache (full-emacs-dir "cache/eln-cache"))
(setq treesit-extra-load-path (list (full-emacs-dir "cache/tree-sitter")))

(with-eval-after-load 'url
  (setq url-cache-directory (full-emacs-dir "cache/url/cache")
        url-cookie-file (full-emacs-dir "cache/url/cookies")
        url-history-file (full-emacs-dir "cache/url/history")))

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq default-font (if (eq system-type 'darwin)
                       "Aporetic Serif Mono"
                     "monospace"))
(dolist (font '(default fixed-pitch fixed-pitch-serif))
  (set-face-attribute font nil :font (concat default-font "-16")))
(dolist (charset '(han cjk-misc))
  (set-fontset-font t charset (font-spec :family "LXGW WenKai" :size 16.5)))

(setq default-text-properties '(line-spacing 0.1 line-height 1.1))

(setq use-short-answers t)

(setq font-lock-maximum-decoration t)
(setq font-lock-verbose t)

(setq line-number-mode t)
(setq column-number-mode t)

(setq kill-ring-max 1000)

;; 1mb
(setq read-process-output-max (* 1024 1024))

(setq-default kill-whole-line t)

(setq Man-notify-method 'pushy)

(setq x-gtk-use-system-tooltips nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-to-list 'load-path (full-emacs-dir "site-lisps"))
(require 'setup-packages)
(require 'setup-prog)
(require 'setup-org)
(require 'setup-hydras)
(require 'setup-env nil t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
