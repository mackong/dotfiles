;;; setup-cc.el --- setup for c/c++ development

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for c/c++ development

;;; Code:
(defun font-lock-if0 (limit)
  "Add font-lock for #if 0."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun setup-c-mode-common ()
  "Setup for cc-mode-common-hook."
  (setq gdb-many-windows t)
  (setq gdb-use-separate-io-buffer t)

  (font-lock-add-keywords
   nil
   '((font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)

  (if (not (string-equal major-mode "glsl-mode"))
      (lsp-cquery-enable))

  (hs-minor-mode 1)
  (add-to-list 'hs-special-modes-alist
               '(c-mode "[\n\t ]*{" "}" "/[*/]" nil nil))
  (local-set-key (kbd "M-+") 'hs-toggle-hiding)

  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (c-toggle-hungry-state 1)
  (c-set-offset 'cpp-define-intro 2)
  (c-set-offset 'statement-case-open '+)
  (setq c-auto-newline nil
        c-electric-pound-behavior '(alignleft)
        c-indent-level 4
        c-brace-imaginary-offset 0
        c-brace-offset -4
        c-argdecl-indent 4
        c-label-offset -4
        c-continued-statement-offset 4
        indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4))

(defun setup-c-mode ()
  "Setup for c mode."
  (setup-c-mode-common)
  (c-set-style "linux"))

(defun setup-c++-mode ()
  "Setup for c++ mode."
  (setup-c-mode-common)
  (c-set-style "stroustrup"))

(add-hook 'c-mode-hook 'setup-c-mode)
(add-hook 'c++-mode-hook 'setup-c++-mode)

(defun create-dot-cquery ()
  "Create a default .cquery in current project root."
  (interactive)
  (with-current-buffer (find-file (concat (projectile-project-root) ".cquery"))
    (insert "%clang
%c -std=gnu11
%cpp -std=gnu++14
-pthread")))

(provide 'setup-cc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-cc.el ends here
