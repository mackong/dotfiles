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

(defun setup-org-jira ()
  "Setup org jira."
  (setq jiralib-url "http://jira.vpgame.cn")
  (setq org-jira-working-dir "~/Documents/Orgs/agenda")
  (setq org-jira-jira-status-to-org-keyword-alist
        '(("To Do" . "TODO")
          ("In Progress" . "NEXT")
          ("Done" . "DONE")))
  (setq org-jira-priority-to-org-priority-alist
        '(("Highest" . ?A)
          ("High" . ?A)
          ("Medium" . ?B)
          ("Low" . ?C)
          ("Lowest" . ?C)))
  (setq org-jira-default-jql "project = DATAPLAT AND assignee = \"kongyichao@vpgame.cn\" ORDER BY Rank ASC"))

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

(defun setup-org-mode ()
  "Setup org mode."
  (org-bullets-mode)

  (setup-org-babel)

  (setq org-export-backends '(ascii beamer html latex man md))

  (setq truncate-lines nil)
  (setq org-use-speed-commands t)
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"))
  (setq org-latex-listings t)
  (setq org-src-preserve-indentation t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq-local company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|-\\)")
  (setq-local truncate-lines t)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (local-set-key (kbd "C-c C-j") 'counsel-imenu))

(setup-org-jira)
(setup-org-agenda)
(setup-org-capture)
(add-hook 'org-mode-hook 'setup-org-mode)

(provide 'setup-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-org.el ends here
