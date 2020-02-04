;;; setup-tools.el --- setup for various tools.

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for various tools.

;;; Code:

;; Shell Path
(setq exec-path-from-shell-check-startup-files nil
      exec-path-from-shell-variables '("PATH" "MANPATH"
                                       "WORKON_HOME"
                                       "JAVA_HOME"
                                       "GOPATH" "GOROOT" "GO111MODULE"))
(exec-path-from-shell-initialize)

;; Theme
(load-theme 'gruber-darker t)

;; linum-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; undo-tree
(global-undo-tree-mode)

;; google-translate
(setq google-translate-translation-directions-alist
      '(("en" . "zh-CN")
        ("zh-CN" . "en")))

;; bongo
(setq bongo-default-directory (expand-file-name "~/Music")
      bongo-insert-whole-directory-trees t)

;; rfc
(setq irfc-directory "~/Documents/RFC/")
(setq irfc-assoc-mode t)
(setq rfc-mode-directory (expand-file-name "~/Documents/RFC/"))
(require 'rfc-mode)
(add-hook 'rfc-mode-hook '(lambda () (irfc-mode)))

;; window-number
(autoload 'window-number-mode "window-number" t)
(autoload 'window-numbermeta-mode "windows-number" t)
(window-number-mode 1)
(window-number-meta-mode 1)
(custom-set-faces
 '(window-number-face ((t nil)) t))

;; yasnippet
(setq yas-prompt-functions '(yas-dropdown-prompt))
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; graphviz-dot-mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" "Graphviz major mode." t)

;; multiple-cursors
(setq mc/list-file "~/.emacs.d/others/.mc-lists.el")

;; nXML
(custom-set-variables
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t))
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"
               "<!--"
               sgml-skip-tag-forward
               nil))
(add-hook 'nxml-mode-hook
          (lambda ()
            (hs-minor-mode)
            (setq indent-tabs-mode nil)))

;; find file as root
(defun find-file-as-root ()
  "Find file as root.

Like `find-file, but automatically edit the file with
root-previleges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (read-file-name "Edit As Root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;; helm
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-occur)
(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t
      helm-buffer-max-length nil)

;; helm-xref
(require 'helm-xref)
(setq-default xref-prompt-for-identifier nil)
(setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long)

;; projectile
(setq projectile-keymap-prefix (kbd "C-c p")
      projectile-completion-system 'helm
      projectile-cache-file "~/.emacs.d/others/projectile/projectile.cache"
      projectile-known-projects-file "~/.emacs.d/el-get/projectile/projectile-bookmarks.eld")
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-globally-ignored-directories
      (append '("bin" "pkg" "vendor"  ;; for golang workspaces
                "project" "target" ".settings"  ;; for maven project
                ".metals" ".bloop"    ;; for sbt project
                )
              projectile-globally-ignored-directories))

;; company
(dolist (hook '(prog-mode-hook
                org-mode-hook
                jupyter-repl-mode-hook))
  (add-hook hook 'company-mode))
(setq company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-show-numbers t
      company-idle-delay .1
      company-echo-delay 0
      company-backends '(company-nxml company-css company-semantic company-cmake company-capf company-files
                                      (company-dabbrev-code company-keywords) company-dabbrev))
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(global-set-key (kbd "C-c y") 'company-yasnippet)

;; lsp
(add-hook 'lsp-before-initialize-hook
          (lambda ()
            (setq lsp-keep-workspace-alive nil
                  lsp-eldoc-render-all nil
                  lsp-signature-render-documentation nil
                  lsp-prefer-flymake nil
                  lsp-ui-doc-enable nil
                  lsp-ui-peek-enable nil
                  lsp-ui-imenu-enable nil
                  lsp-ui-sideline-show-code-actions nil
                  lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-show-hover nil)
            (local-set-key (kbd "C-.") 'xref-find-definitions)
            (local-set-key (kbd "C-,") 'xref-pop-marker-stack)))
(setq imenu-max-item-length 'Unlimited)

;; multi scratch
(require 'multi-scratch)

;; multi term
(cl-flet ((set-color (pair)
                     (multiple-value-bind (face color)
                         pair
                       (set-face-attribute face
                                           nil
                                           :foreground color
                                           :background nil))))
  (mapc #'set-color
        '((term-color-black "#2e3434")
          (term-color-red "tomato")
          (term-color-green "#6ac214")
          (term-color-yellow "#edd400")
          (term-color-blue "light sky blue")
          (term-color-magenta "magenta")
          (term-color-cyan "cyan")
          (term-color-white "#eeeeec"))))

(setq-default ansi-term-color-vector
              [term-face
               term-color-black
               term-color-red
               term-color-green
               term-color-yellow
               term-color-blue
               term-color-magenta
               term-color-cyan
               term-color-white])

(add-hook 'term-mode-hook
          (lambda ()
            (copy-face 'default 'term-face)

            ;; Disable hl-line-mode
            (setq-local global-hl-line-mode nil)

            ;; Disable yasnippet
            (yas-minor-mode -1)

            ;; awesome bindings available!
            (compilation-shell-minor-mode t)))

;; dired
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 0)))
(diredp-toggle-find-file-reuse-dir 1)
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-dwim-target t)

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

;; pdf-tools
(pdf-loader-install)
(add-hook 'pdf-tools-enabled-hook
          '(lambda ()
             (setq pdf-view-display-size 'fit-page
                   pdf-view-resize-factor 1.1)))

;; cmake-mode
(require 'cmake-mode nil 'noerror)

(provide 'setup-tools)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-tools.el ends here
