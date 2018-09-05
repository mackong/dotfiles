;;; init.el --- startup file for emacs

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Startup file for Emacs.

;;; Code:

(package-initialize)

(setq frame-title-format
      '("emacs@" (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")) " [%*]"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq-default cursor-type 'box)

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

(let ((frame-font "Fira Code-16:weight=normal")
      (han-font-family "KaiTi")
      (han-font-size 20))
  (if (string= system-type "darwin")
      (progn
        (setq frame-font "SF Mono-16")
        (setq han-font-family "Kaiti SC")
        (setq han-font-size 18)))
  (set-frame-font frame-font)
  (set-fontset-font t 'han (font-spec :family han-font-family :size han-font-size)))

(fset 'yes-or-no-p 'y-or-n-p)

(setq font-lock-maximum-decoration t)
(setq font-lock-global-mode t)
(setq font-lock-verbose t)

(global-hl-line-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

(setq kill-ring-max 1000)

(setq-default kill-whole-line t)

(setq Man-notify-method 'pushy)

(setq x-gtk-use-system-tooltips nil)

(setq browse-url-browser-function 'eww-browse-url)

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
(require 'setup-java)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
