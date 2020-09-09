;;; setup-hydra.el --- setup for various hydras.

;; Copyright (C) 2020 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.dotfiles

;;; Commentary:

;; Setup for various hydras.

;;; Code:

;;; run function and move to other window
(defun run-in-other-window (func)
  (interactive)
  (call-interactively func)
  (other-window 1))

;;; describe this point lisp only
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
                           (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

(defun fake-mode-hydra ()
  (interactive)
  (major-mode-hydra-dispatch 'fake-mode))
(global-set-key (kbd "C-M-,") 'fake-mode-hydra)
(global-set-key (kbd "C-SPC") 'major-mode-hydra)

(major-mode-hydra-define fake-mode nil
  ("Dash"
   (("di" dash-docs-install-docset "install")
    ("da" dash-docs-activate-docset "activate")
    ("dh" helm-dash-at-point "dash at point")
    ("dd" helm-dash "dash"))
   "Tools"
   (("bg" bongo-playlist "bongo")
    ("gt" (run-in-other-window 'google-translate-smooth-translate) "google translate")
    ("ms" magit-status "magit")
    ("mt" multi-term "terminal")
    ("sn" (multi-scratch-new t) "scratch"))
   "Runners"
   (("rp" (run-in-other-window 'run-python) "python")
    ("rl" slime "common lisp")
    ("rs" run-geiser "geiser"))
   "Misc"
   (("zz" text-scale-adjust "zoom")
    ("ci" change-inner "change inner")
    ("co" change-outer "change outer")
    ("vr" vr/replace "visual regexp")
    ("tm" treemacs "treemacs"))))

(major-mode-hydra-define pdf-view-mode nil
  ("Move"
   (("n" pdf-view-next-page-command "next page")
    ("p" pdf-view-previous-page-command "previous page")
    ("g" pdf-view-first-page "first page")
    ("G" pdf-view-last-page "last page")
    ("e" pdf-view-goto-page "goto page")
    ("B" pdf-history-backward "history backward")
    ("N" pdf-history-forward "history forward"))
   "Scale/Fit"
   (("+" pdf-view-enlarge "enlarge")
    ("-" pdf-view-shrink "shrink")
    ("0" pdf-view-scale-reset "reset")
    ("H" pdf-view-fit-height-to-window "fit height")
    ("W" pdf-view-fit-width-to-window "fit width")
    ("P" pdf-view-fit-page-to-window "fit page"))
   "Annotations"
   (("al" pdf-annot-list-annotations "list")
    ("ad" pdf-annot-delete "delete")
    ("aa" pdf-annot-attachment-dired "attachment dired")
    ("am" pdf-annot-add-markup-annotation "add markup")
    ("at" pdf-annot-add-text-annotation "add text"))
   "Search/Link"
   (("s" isearch-forward "search")
    ("S" pdf-occur "occur")
    ("o" pdf-outline "outline")
    ("F" pdf-links-action-perfom "link")
    ("f" pdf-links-isearch-link "search link"))
   "Misc"
   (("u" pdf-view-revert-buffer "revert buffer")
    ("i" pdf-misc-display-metadata "info")
    ("d" pdf-view-dark-minor-mode "dark mode")
    ("m" pdf-view-midnight-minor-mode "midnight mode"))))

;;; for lsp-mode
(major-mode-hydra-define (java-mode scala-mode python-mode c-mode c++-mode go-mode) nil
  ("Symbol"
   (("d" lsp-find-declaration "declaration")
    ("t" lsp-find-definition "definition")
    ("R" lsp-find-references "references")
    ("i" lsp-find-implementation "implementation")
    ("o" (run-in-other-window 'lsp-describe-thing-at-point) "documentation")
    ("r" lsp-rename "rename"))
   "Buffer"
   (("f" lsp-format-buffer "format")
    ("e" lsp-treemacs-errors-list "errors")
    ("m" helm-imenu "imenu")
    ("l" lsp-treemacs-symbols "symbols")
    ("x" lsp-execute-code-action "code action"))
   "Server"
   (("S" lsp-shutdown-workspace "shutdown")
    ("M-r" lsp-restart-workspace "restart")
    ("M-s" lsp-describe-session "describe session"))))

(major-mode-hydra-define+ emacs-lisp-mode nil
  ("Compile"
   (("c" (byte-compile-file (buffer-file-name)) "this file"))
   "Eval"
   (("b" eval-buffer "buffer"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" (run-in-other-window 'describe-foo-at-point) "thing-at-pt")
    ("f" (run-in-other-window 'describe-function) "function")
    ("v" (run-in-other-window 'describe-variable) "variable")
    ("i" (run-in-other-window 'info-lookup-symbol) "info lookup"))))

(major-mode-hydra-define+ org-mode nil
  ("Table"
   (("ic" org-table-insert-column "insert column")
    ("ira" org-table-insert-row "insert row above")
    ("irb" (org-table-insert-row t) "insert row below")
    ("dc" org-table-delete-column "delete column")
    ("ml" org-table-move-column-left "move column left")
    ("mr" org-table-move-column-right "move column right")
    ("mu" org-table-move-row-up "move row up")
    ("md" org-table-move-row-down "move row down"))))

(provide 'setup-hydras)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-hydras.el ends here
