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
(load-theme 'cyberpunk t)

;; smart-mode-line
(setq rm-blacklist
      (format "^ \\(%s\\)$"
              (mapconcat #'identity
                         '("Abbrev.*" "Fly.*" "Projectile.*" "Undo-Tree.*" "company.*"
                           "Helm.*" "ElDoc.*" "ws.*" "doxy.*" "Paredit.*")
                         "\\|")))
(setq sml/shorten-directory t
      sml/shorten-modes t
      sml/no-confirm-load-theme t
      sml/theme 'dark)
(sml/setup)

;; linum-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; undo-tree
(global-undo-tree-mode)

;; google-translate
(setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
(global-set-key (kbd "C-c g t")
                '(lambda ()
                   (interactive)
                   (google-translate-smooth-translate)
                   (other-window 1)))

;; bongo
(setq bongo-default-directory (expand-file-name "~/Music")
      bongo-insert-whole-directory-trees t)

;; symbol overlay
(global-set-key [f12] 'symbol-overlay-put)

;; rfc
(setq irfc-directory "~/Documents/RFC/")
(setq irfc-assoc-mode t)
(setq rfc-mode-directory (expand-file-name "~/Documents/RFC/"))
(require 'rfc-mode)
(add-hook 'rfc-mode-hook '(lambda () (irfc-mode)))

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

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

(defvar yas/temp-snippet nil "Content of the temporary snippet.")

(defun yas/save-temp-snippet ()
  "Save the temporary snippet."
  (interactive)
  (setq yas/temp-snippet
        (buffer-substring (region-beginning) (region-end))))

(defun yas/expand-temp-snippet ()
  "Expand the temporary snippet."
  (interactive)
  (yas/expand-snippet yas/temp-snippet))

;; graphviz-dot-mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode.el" "Graphviz major mode." t)

;; multiple-cursors
(setq mc/list-file "~/.emacs.d/others/.mc-lists.el")
(global-set-key (kbd "C-c m m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m e") 'mc/edit-lines)

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

;; gnuplot
(setq gnuplot-program "/usr/bin/gnuplot")
(setq auto-mode-alist
      (append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))

;; find file as root
(defun find-file-as-root ()
  "Find file as root.

Like `find-file, but automatically edit the file with
root-previleges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
(global-set-key (kbd "C-x F") 'find-file-as-root)

;; ascii doc
(autoload 'adoc-mode "adoc-mode")
(add-to-list 'auto-mode-alist (cons "\\.asc\\'" 'adoc-mode))

;; dpaste.el
(setq dpaste-poster "mackong(mackonghp@gmail.com)")

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; change-inner
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'change-inner)
(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'change-outer)

;; magit
(global-set-key (kbd "C-c m s") 'magit-status)

;; helm
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-occur)
(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)

;; projectile
(setq projectile-keymap-prefix (kbd "C-c p"))
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm
      projectile-cache-file "~/.emacs.d/others/projectile/projectile.cache"
      projectile-known-projects-file "~/.emacs.d/el-get/projectile/projectile-bookmarks.eld"
      projectile-globally-ignored-directories (append '("bin" "pkg" "vendor"  ;; for golang workspaces
                                                        "project" "target" ".settings"  ;; for maven project
                                                        ".metals" ".bloop"    ;; for sbt project
                                                        ".cquery_cached_index" ;; for cquery cache
                                                        )
                                                      projectile-globally-ignored-directories))

;; company
(dolist (hook '(prog-mode-hook
                org-mode-hook))
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
                  lsp-prefer-flymake nil
                  lsp-ui-doc-enable nil
                  lsp-ui-peek-enable nil
                  lsp-ui-imenu-enable nil
                  lsp-ui-sideline-show-code-actions nil
                  lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-show-hover nil)
            (local-set-key (kbd "C-.") 'xref-find-definitions)
            (local-set-key (kbd "C-,") 'xref-pop-marker-stack)
            (local-set-key (kbd "C-c C-j") 'helm-imenu)))
(setq imenu-max-item-length 'Unlimited)

;; multi scratch
(require 'multi-scratch)
(global-set-key (kbd "C-c s n") 'multi-scratch-new)

;; multi term
(global-set-key (kbd "C-c m t") 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
            (copy-face 'default 'term-face)

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
(require 'dwim-compile)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output 'first-error)
(global-set-key (kbd "C-c d c") 'dwim-c/compile)

;; maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "Large")

;; helm-dash
(global-set-key (kbd "C-c d a") 'helm-dash-activate-docset)
(global-set-key (kbd "C-c d d") 'helm-dash)
(global-set-key (kbd "C-c d h") 'helm-dash-at-point)

;; whitespace mode
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-style '(face tabs empty trailing))

;; pdf-tools
(pdf-loader-install)
(add-hook 'pdf-tools-enabled-hook '(lambda ()
                                     (progn
                                       (setq pdf-view-display-size 'fit-page
                                             pdf-view-resize-factor 1.1)
                                       (pdf-view-midnight-minor-mode 1)
                                       (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))))

(provide 'setup-tools)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-tools.el ends here
