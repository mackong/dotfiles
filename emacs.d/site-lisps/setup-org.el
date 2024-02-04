;;; setup-org.el --- setup for org-mode

;; Copyright (C) 2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/dotfiles/emacs.d

;;; Commentary:

;; Setup for org-mode
;; see http://www.cnblogs.com/visayafan/archive/2012/06/16/2552023.html

;;; Code:

;; see https://emacs-china.org/t/org-mode-latex-mode/22490

;; Vertically align LaTeX preview in org mode
(defun org-latex-preview-advice (beg end &rest _args)
  (let* ((ov (car (overlays-at (/ (+ beg end) 2) t)))
         (img (cdr (overlay-get ov 'display)))
         (new-img (plist-put img :ascent 95)))
    (overlay-put ov 'display (cons 'image new-img))))

(defun org-justify-fragment-overlay (beg end image imagetype)
  (let* ((position (plist-get org-format-latex-options :justify))
         (img (create-image image 'svg t))
         (ov (car (overlays-at (/ (+ beg end) 2) t)))
         (width (car (image-display-size (overlay-get ov 'display))))
         offset)
    (cond
     ((and (eq 'center position)
           (= beg (line-beginning-position)))
      (setq offset (floor (- (/ fill-column 2)
                             (/ width 2))))
      (if (< offset 0)
          (setq offset 0))
      (overlay-put ov 'before-string (make-string offset ? )))
     ((and (eq 'right position)
           (= beg (line-beginning-position)))
      (setq offset (floor (- fill-column width)))
      (if (< offset 0)
          (setq offset 0))
      (overlay-put ov 'before-string (make-string offset ? ))))))

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
  (global-set-key (kbd "C-c o a") 'org-agenda)

  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

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
  (org-modern-mode)
  (electric-indent-local-mode 1)

  (require 'ox-gfm nil t)

  (setq-local company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|-\\)"
              truncate-lines nil
              indent-tabs-mode nil)

  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (advice-add 'org--make-preview-overlay
              :after 'org-latex-preview-advice)

  (advice-add 'org--make-preview-overlay
              :after 'org-justify-fragment-overlay)

  (local-set-key (kbd "C-c C-j") 'org-goto))

(defun setup-org-mode ()
  "Setup org mode."
  (setup-org-babel)

  (setq org-export-backends '(ascii beamer html latex man md)
        org-use-speed-commands t
        org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f")
        org-latex-listings 'minted
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-format-latex-options (plist-put org-format-latex-options :justify 'center)
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
