;;; setup-tools.el --- setup for various tools.

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for various tools.

;;; Code:

;; Shell Path
(setq exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

;; Theme
(load-theme 'monokai t)

;; linum-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; undo-tree
(global-undo-tree-mode)

;; stardict
(defun stardict-search-wordap (&optional word)
  "Use python script dict to look up WORD under point."
  (interactive)
  (or word (setq word (current-word)))
  (shell-command (format "sdcv -n %s" word)))
(global-set-key (kbd "C-c s d") 'stardict-search-wordap)

;; bongo
(setq bongo-default-directory (expand-file-name "~/Music")
      bongo-insert-whole-directory-trees t)

;; highlight symbol
(global-set-key [f8] 'highlight-symbol-at-point)
(global-set-key [(control f8)] 'highlight-symbol-next)
(global-set-key [(shift f8)] 'highlight-symbol-prev)

;; irfc
(setq irfc-directory "~/Documents/RFC/")
(setq irfc-assoc-mode t)

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
(add-hook 'nxml-mode-hook
          (lambda ()
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

;; rainbow-identifiers
;; from spacemacs
(defun tweak-rainbow-identifiers-color ()
  (loop for i from 1 to 15 do
	(let* ((lightness 55)
               (saturation 40)
               (angle (* 2 pi (/ i 15.0)))
               (a (* saturation (cos angle)))
               (b (* saturation (sin angle))))
          (set-face-attribute
           (intern (format "rainbow-identifiers-identifier-%s" i))
           nil
           :foreground (apply 'color-rgb-to-hex
                              (color-lab-to-srgb lightness a b))))))
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'rainbow-identifiers-mode-hook 'tweak-rainbow-identifiers-color)

;; sr-speedbar
(setq speedbar-show-unknown-files t
      sr-speedbar-width 30
      sr-speedbar-right-side nil
      sr-speedbar-auto-refresh nil)

(global-set-key (kbd "C-c b t") 'sr-speedbar-toggle)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

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
(setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")

;; projectile
(setq projectile-keymap-prefix (kbd "C-c p"))
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm
      projectile-cache-file "~/.emacs.d/others/projectile/projectile.cache"
      projectile-known-projects-file "~/.emacs.d/el-get/projectile/projectile-bookmarks.eld"
      projectile-globally-ignored-directories (append '("bin" "pkg" "vendor"  ;; for golang workspaces
                                                        "target" ".settings"  ;; for maven project
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
(require 'lsp-imenu)
(add-hook 'lsp-after-open-hook
          (lambda ()
          (setq lsp-enable-completion-at-point t
                lsp-hover-text-function 'lsp--text-document-signature-help
                lsp-enable-eldoc t
                lsp-inhibit-message t
                lsp-ui-doc-enable nil
                lsp-ui-peek-enable nil
                lsp-ui-sideline-enable nil
                lsp-ui-imenu-enable nil
                lsp-ui-flycheck-enable t)
          (lsp-enable-imenu)
          (lsp-ui-mode)
          (push 'company-lsp company-backends)))
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

;; editorconfig
(add-hook 'prog-mode-hook 'editorconfig-mode)

;; smooth scrolling
(smooth-scrolling-mode 1)

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

;; flycheck
(add-hook 'prog-mode-hook 'flycheck-mode)

(provide 'setup-tools)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-tools.el ends here
