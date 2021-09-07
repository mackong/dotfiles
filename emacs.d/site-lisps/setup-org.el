;;; setup-org.el --- setup for org-mode

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

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
     (clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (java . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (maxima . t)
     (plantuml . t)
     (python . t)
     (R . t)
     (ruby . t)
     (scheme . t)
     (shell . t)
     (sql . t)
     (restclient . t)
     (go . t)))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-plantuml-jar-path plantuml-jar-path))

(defun setup-org-agenda ()
  "Setup org agenda."
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red")
          ("NEXT" :foreground "deep sky blue")
          ("DONE" :foreground "dark red")
          ("HOLD" :foreground "yellow")
          ("CANCELLED" :foreground "forest green")))
  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority TODOS:")))
            (agenda "")
            (alltodo "")))))
  (setq org-agenda-files '("~/Documents/Orgs/agenda"))
  (global-set-key (kbd "C-c o a") 'org-agenda))

(defun setup-org-capture ()
  "Setup org capture."
  (setq org-default-notes-file "~/Documents/Orgs/agenda/journal.org")
  (setq org-capture-templates
        '(("t" "task" entry (file "~/Documents/Orgs/agenda/task.org")
           "* TODO %?\n" :jump-to-captured t)
          ("n" "note" entry (file "~/Documents/Orgs/agenda/journal.org")
           "* %?\n" :jump-to-captured t)
          ("r" "reminder" entry (file "~/Documents/Orgs/agenda/someday.org")
           "* %?\n" :jump-to-captured t)
          ))
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

(defun setup-org-mode ()
  "Setup org mode."
  (org-bullets-mode)

  (setup-org-babel)

  (setq org-export-backends '(ascii beamer html latex man md))

  (setq truncate-lines nil
        org-use-speed-commands t
        org-latex-pdf-process '("xelatex -interaction nonstopmode %f")
        org-latex-listings t
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-goto-interface 'outline-path-completionp
        org-outline-path-complete-in-steps nil
        org-archive-location "~/Documents/Orgs/agenda/archive.org::")
  (setq-local company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|-\\)")
  (setq-local truncate-lines t)
  (setq-local indent-tabs-mode nil)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (local-set-key (kbd "C-c C-j") 'org-goto))

(setup-org-agenda)
(setup-org-capture)
(add-hook 'org-mode-hook 'setup-org-mode)

(provide 'setup-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-org.el ends here
