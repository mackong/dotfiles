;;; setup-hydra.el --- setup for various hydras.

;; Copyright (C) 2020 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/mackong/.dotfiles

;;; Commentary:

;; Setup for various hydras.

;;; Code:

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
   "Avy"
   (("gc" avy-goto-char "goto char")
    ("gl" avy-goto-line "goto line")
    ("gw" avy-goto-word-1 "goto word"))
   "Tools"
   (("bg" bongo-playlist "bongo")
    ("gt" (progn (google-translate-smooth-translate) (other-window 1)) "google translate")
    ("ms" magit-status "magit")
    ("mt" multi-term "terminal")
    ("sn" (multi-scratch-new t) "scratch"))
   "Runners"
   (("rp" run-python "python")
    ("rj" jupyter-run-repl "jupyter repl")
    ("rg" go-playground "go playground")
    ("rl" slime "common lisp"))
   "Misc"
   (("zz" text-scale-adjust "zoom")
    ("er" er/expand-region "expand region")
    ("ci" change-inner "change inner")
    ("co" change-outer "change outer")
    ("ma" mc/mark-all-like-this "mark all like this")
    ("me" mc/edit-lines "mark lines to edit")
    ("vr" vr/replace "visual regexp")
    ("fr" find-file-as-root "edit as root"))))

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
    ("D" lsp-ui-peek-find-definitions "definition")
    ("R" lsp-ui-peek-find-references "references")
    ("i" lsp-ui-peek-find-implementation "implementation")
    ("t" lsp-find-type-definition "type")
    ("s" lsp-signature-help "signature")
    ("o" lsp-describe-thing-at-point "documentation")
    ("r" lsp-rename "rename"))
   "Buffer"
   (("f" lsp-format-buffer "format")
    ("m" helm-imenu "imenu")
    ("x" lsp-execute-code-action "code action"))
   "Server"
   (("S" lsp-shutdown-workspace "shutdown")
    ("M-r" lsp-restart-workspace "restart")
    ("M-s" lsp-describe-session "describe session"))))

(major-mode-hydra-define+ emacs-lisp-mode nil
  ("REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(major-mode-hydra-define+ go-mode nil
  ("Playground"
   (("pe" go-playground-exec "execute")
    ("pr" go-playground-rm "remove"))))

(provide 'setup-hydras)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; setup-hydras.el ends here
