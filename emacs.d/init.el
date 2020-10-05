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

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(delete-selection-mode 1)

(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

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
(set-face-attribute 'default nil :font "Fira Code:pixelsize=17")
(set-face-attribute 'fixed-pitch-serif nil :font "Fira Code:pixelsize=17")
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

(setq browse-url-handlers
      '(("^file://.*" . eww)
        ("^https?://.*" . browse-url-chrome)))

(setq url-proxy-services '(("http" . "127.0.0.1:12345")
                           ("https" . "127.0.0.1:12345")
                           ("no_proxy" . "\\(.*\\.vpgame\\.cn\\|localhost\\|127\\.0\\.0\\.1\\|mirrors.tuna.tsinghua.edu.cn\\)")))

;; linum-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

;; compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error)

;; maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "Large")

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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; init.el ends here
