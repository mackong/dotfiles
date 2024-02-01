;;; setup-org.el --- setup for org-mode

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/dotfiles/emacs.d

;;; Commentary:

;; Setup for org-mode
;; see http://www.cnblogs.com/visayafan/archive/2012/06/16/2552023.html

;;; Code:

(defun setup-org-babel ()
  "Setup org babel."
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (dot . t)
     (emacs-lisp . t)
     (go . t)
     (ipython . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (maxima . t)
     (plantuml . t)
     (python . t)
     (restclient . t)
     (ruby . t)
     (scheme . t)
     (shell . t)
     (sql . t)))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (setq org-confirm-babel-evaluate nil
        org-plantuml-jar-path plantuml-jar-path)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

(defun setup-org-agenda ()
  "Setup org agenda."
  (setq org-agenda-files '("~/Documents/Orgs/agenda")
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("NEXT" . '(bold shadow))
                                 ("HOLD" . '(bold org-todo))
                                 ("CANCELLED" . '(bold org-done)))
        org-agenda-custom-commands '(("c" "Simple agenda view"
                                      ((tags "PRIORITY=\"A\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "High-priority TODOS:")))
                                       (agenda "")
                                       (alltodo "")))))
  (global-set-key (kbd "C-c o a") 'org-agenda))

(defun setup-org-capture ()
  "Setup org capture."
  (setq org-default-notes-file "~/Documents/Orgs/agenda/journal.org"
        org-capture-templates '(("t" "task" entry (file "~/Documents/Orgs/agenda/task.org")
                                 "* TODO %?\n" :jump-to-captured t)
                                ("n" "note" entry (file "~/Documents/Orgs/agenda/journal.org")
                                 "* %?\n" :jump-to-captured t)
                                ("r" "reminder" entry (file "~/Documents/Orgs/agenda/someday.org")
                                 "* %?\n" :jump-to-captured t)
                                ("d" "New note (with denote.el)" plain
                                 (file denote-last-path)
                                 #'denote-org-capture
                                 :no-save t
                                 :immediate-finish nil
                                 :kill-buffer t
                                 :jump-to-captured t)))
  (global-set-key (kbd "C-c o c") 'org-capture))

(defun archive-done-tasks ()
  "Archive done tasks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))

(defun setup-org-mode-hook ()
  "Hook function for org-mode-hook"
  (org-bullets-mode)
  (electric-indent-local-mode 1)

  (require 'ox-gfm nil t)

  (setq-local company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|-\\)"
              truncate-lines nil
              indent-tabs-mode nil)

  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (local-set-key (kbd "C-c C-j") 'org-goto))

(defun setup-org-mode ()
  "Setup org mode."
  (setup-org-babel)

  (setq org-export-backends '(ascii beamer html latex man md)
        org-use-speed-commands t
        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f")
        org-latex-listings 'minted
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-archive-location "~/Documents/Orgs/agenda/archive.org::"
        org-adapt-indentation t
        org-goto-interface 'outline-path-completionp
        org-outline-path-complete-in-steps nil
        org-highlight-latex-and-related '(latex))

  (add-hook 'org-mode-hook 'setup-org-mode-hook))

(setup-org-agenda)
(setup-org-capture)
(setup-org-mode)

(provide 'setup-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-org.el ends here
