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
	      " ")))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(delete-selection-mode 1)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq-default cursor-type 'hbar)
(setq default-text-properties '(line-spacing 0.25 line-height 1.25))

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-default-init t)

(setq user-full-name "mackong")
(setq user-mail-address "mackonghp@gmail.com")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq recentf-save-file (expand-file-name "~/.emacs.d/others/recentf"))
(setq create-lockfiles nil)

(setq eshell-directory-name (expand-file-name "~/.emacs.d/others/eshell/"))
(setq bookmark-save-flag nil)
(setq tramp-ssh-controlmaster-options "")

(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq face-font-rescale-alist '(("微软雅黑" . 1.2)))
(set-face-attribute 'default nil :font "DejaVu Sans Mono:pixelsize=17")
(set-face-attribute 'fixed-pitch-serif nil :font "DejaVu Sans Mono:pixelsize=17")
(set-fontset-font t 'symbol (font-spec :family "Unifont") nil 'append)
(set-fontset-font t nil (font-spec :family "DejaVu Sans"))
(dolist (charset '(han cjk-misc))
  (set-fontset-font t charset (font-spec :family "Microsoft Yahei")))

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

(setq browse-url-browser-function
      '(("^file://.*" . eww)
        ("^https?://.*" . browse-url-chrome)))

(setq url-proxy-services '(("http" . "127.0.0.1:12345")
                           ("https" . "127.0.0.1:12345")
                           ("no_proxy" . "^\\(vpgame\\.cn\\|localhost\\|127\\.0\\.0\\.1\\)")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisps"))
(require 'setup-el-get)
(require 'setup-tools)
(require 'setup-cc)
(require 'setup-lisp)
(require 'setup-tex)
(require 'setup-python)
(require 'setup-org)
(require 'setup-web)
(require 'setup-go)
(require 'setup-scala)
(require 'setup-hydras)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
