;; -*- lexical-binding: t; -*-

;;; early-init.el --- runs before package/UI init

;;; Commentary:

;;; Code:

(setq package-enable-at-startup nil)
(setq site-run-file nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my--file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
