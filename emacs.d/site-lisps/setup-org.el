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

  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-results-keyword "results")
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-plantuml-jar-path (expand-file-name
                               (if (string= system-type "darwin")
                                   "~/Library/Plantuml/plantuml.jar"
                                 "~/Tools/plantuml/plantuml.jar"))))

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

  (setq org-agenda-files '("~/Dropbox/Orgs/agenda" "~/Documents/Orgs/agenda")))

(defun setup-org-capture ()
  "Setup org capture."
  (setq org-default-notes-file "~/Dropbox/Orgs/notes/notes.org")
  (setq org-capture-templates
        '(("t" "todo" entry (file "~/Dropbox/Orgs/agenda/todo.org")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file "~/Dropbox/Orgs/notes/notes.org")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))

(defun setup-org-mode ()
  "Setup org mode."
  (turn-on-org-cdlatex)
  (org-bullets-mode 1)
  (setup-org-babel)
  (setq truncate-lines nil)
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"))
  (setq org-latex-listings t)
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(setup-org-agenda)
(setup-org-capture)
(add-hook 'org-mode-hook 'setup-org-mode)

(provide 'setup-org)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-org.el ends here
