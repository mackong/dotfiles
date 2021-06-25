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

(major-mode-hydra-define fake-mode
  (:quit-key ("q" "<escape>"))
  ("EAF"
   (("eh" eaf-open-browser-with-history "history")
    ("eb" eaf-open-bookmark "bookmark")
    ("et" eaf-open-terminal "terminal")
    ("ej" eaf-open-jupyter "jupyter")
    ("em" eaf-create-mindmap "mindmap")
    ("eq" eaf-stop-process "quit"))
   "Tools"
   (("bg" bongo-playlist "bongo")
    ("gt" (run-in-other-window 'go-translate) "google translate")
    ("ms" magit-status "magit")
    ("sn" (multi-scratch-new t) "scratch")
    ("tm" treemacs "treemacs")
    ("cd" counsel-dash "dash"))
   "Misc"
   (("zz" text-scale-adjust "zoom")
    ("ci" change-inner "change inner")
    ("co" change-outer "change outer")
    ("vr" vr/replace "visual regexp"))))

;;; for lsp-mode
(major-mode-hydra-define (java-mode scala-mode python-mode c-mode c++-mode go-mode)
  (:quit-key ("q" "<escape>"))
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
    ("m" counsel-imenu "imenu")
    ("l" lsp-treemacs-symbols "symbols")
    ("x" lsp-execute-code-action "code action"))
   "Workspace"
   (("S" lsp-ivy-workspace-symbol "workspace symbols")
    ("M-S" lsp-shutdown-workspace "shutdown")
    ("M-r" lsp-restart-workspace "restart")
    ("M-s" lsp-describe-session "describe session"))))

(major-mode-hydra-define+ emacs-lisp-mode
  (:quit-key ("q" "<escape>"))
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

(provide 'setup-hydras)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-hydras.el ends here
