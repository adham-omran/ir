;;; ir-on-hold.el --- On Hold Functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/adham/ir-on-hold
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Features in this file are blocked by some task I have not yet finished or
;; some workflow I have not yet decided on.
;;
;; Cloze Functions: I have not decided on a workflow for how clozes are created
;; and sent to Anki. The *simplest* approach would be to have no cloze functions
;; and use ankifier to create clozes.
;;
;; Highlighting Functions: I have no idea how to create permenant highlight.
;; This would require investigation into Vanilla Emacs and a 2004 archive.
;;
;;; Code:

                                        ; Cloze Functions
;; There are multiple ways to cloze
;;
;; 1. Creating a single cloze deletion: This is the simplest and requires just
;; running (ankifier-create-cloze-from-selection) on the text
;;
;; 2. Creating multiple clozes: just normally create the clozes with (anki-editor-cloze-dwim)

;; (defun ir-cloze-add-cloze ()
;;   (interactive)
;;   "Create a cloze deletion but does not commit. Increments."
;;   (anki-editor-cloze-dwim))

;; (defun ir--commit-cloze ()
;;   "Convert a region of text with cloze deletion into anki clozes.")

;; (defun ir-cloze-single ()
;;   (interactive)
;;   "Create a single cloze and commits it."
;;   ;; TODO Where should I define this? this is more of anki-editor-extra
;;   (anki-editor-cloze-region-dont-incr))

;; (defun ir--get-text-from-org-heading ()
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading)
;;     (forward-line 4) ;; This assumes no other PROPERTIES will be included except the id.
;;     (unless (= (point) (point-max))
;;       (let ((b (point))
;;             (e (or (outline-next-heading) (point-max))))
;;         (message "%s" (buffer-substring-no-properties b e))))))


                                        ; Highlighting
;; (setq hlt-use-overlays-flag nil)
;; (setq hlt-face-prop 'face)
;; ;;
;; I want to highlight text
;;
;; TODO Figure out how to do permentant highlights.
;; (defun remove-display-text-property (start end)
;;   "Remote all text properties from START to END.
;; This is useful when copying stuff with a display property set
;; from elsewhere."
;;   (interactive "r")
;;   (set-text-properties start end nil())
;;   (add-text-properties start end '(font-lock-ignore t face hi-yellow hlt-highlight hi-yellow)))

;; (defun perm-highlight (start end)
;;   (interactive "r")
;;   (hlt-highlight-region start end)
;;   (add-text-properties start end '(font-lock-ignore t)))

;; (defun tell-props ()
;;   (interactive)
;;   (message "%s" (text-properties-at (point))))

(provide 'ir-on-hold)
;;; ir-on-hold.el ends here
