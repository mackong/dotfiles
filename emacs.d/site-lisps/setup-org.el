;;; setup-org.el --- setup for org-mode

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.emacs.d

;;; Commentary:

;; Setup for org-mode
;; see http://www.cnblogs.com/visayafan/archive/2012/06/16/2552023.html

;;; Code:
(custom-set-variables
 '(org-export-backends '(ascii beamer html latex man md)))

(add-to-list 'load-path "~/.emacs.d/el-get/cdlatex-mode")
(require 'ox-latex)

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
     (plantuml . t)
     (python . t)
     (ipython . t)
     (R . t)
     (ruby . t)
     (scheme . t)
     (shell . t)
     (sql . t)))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-results-keyword "results")
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-plantuml-jar-path plantuml-jar-path))

(defun setup-org-agenda ()
  "Setup org agenda."
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red")
          ("NEXT" :foreground "blue")
          ("DONE" :foreground "forest green")
          ("WAITING" :foreground "orange")
          ("HOLD" :foreground "magenta")
          ("CANCELLED" :foreground "forest green")
          ("MEETING" :foreground "forest green")
          ("PHONE" :foreground "forest green")))

  (setq org-agenda-files '("~/MEGA/Orgs/agenda" "~/Documents/Orgs/agenda"))
  (global-set-key (kbd "C-c o a") 'org-agenda))

(defun setup-org-capture ()
  "Setup org capture."
  (setq org-default-notes-file "~/MEGA/Orgs/notes/notes.org")
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/MEGA/Orgs/agenda/todo.org")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file "~/MEGA/Orgs/notes/notes.org")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)))
  (global-set-key (kbd "C-c o c") 'org-capture))

(defun setup-org-mode ()
  "Setup org mode."
  (turn-on-org-cdlatex)
  (setup-org-babel)
  (setq truncate-lines nil)
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"))
  (setq org-latex-listings t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (local-set-key (kbd "C-c C-j") 'helm-imenu))

(setup-org-agenda)
(setup-org-capture)
(add-hook 'org-mode-hook 'setup-org-mode)

(provide 'setup-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-org.el ends here
