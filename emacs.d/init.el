;;; init.el --- startup file for emacs

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Startup file for Emacs.

;;; Code:

(setq frame-title-format
      '("emacs@" (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))

(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(delete-selection-mode 1)

(setq-default cursor-type 'box)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-default-init t)

(setq user-full-name "mackong")
(setq user-mail-address "mackonghp@gmail.com")

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq recentf-save-file (expand-file-name "~/.emacs.d/others/recentf"))
(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq eshell-directory-name (expand-file-name "~/.emacs.d/others/eshell/"))
(setq bookmark-save-flag nil)
(setq bookmark-default-file (expand-file-name "~/.emacs.d/others/bookmarks"))
(setq tramp-ssh-controlmaster-options "")

(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute 'default nil :font "Sarasa Fixed Slab SC 14")
(setq default-text-properties '(line-spacing 0.1 line-height 1.1))

(fset 'yes-or-no-p 'y-or-n-p)

(setq font-lock-maximum-decoration t)
(setq font-lock-global-mode t)
(setq font-lock-verbose t)

(global-hl-line-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

(setq kill-ring-max 1000)

; 100 mb
(setq gc-cons-threshold 100000000)

;; 1mb
(setq read-process-output-max (* 1024 1024))

(setq-default kill-whole-line t)

(setq Man-notify-method 'pushy)

(setq x-gtk-use-system-tooltips nil)

;; compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error)

;; imenu
(setq imenu-max-item-length 'Unlimited)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisps"))
(require 'setup-packages)
(require 'setup-cc)
(require 'setup-lisp)
(require 'setup-python)
(require 'setup-org)
(require 'setup-go)
(require 'setup-hydras)
(require 'setup-env nil t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
