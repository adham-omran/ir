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
                                        ; UI

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-example-repeat)

(defun widget-example ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer "*Widget Example*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v " ; Text after the field!
                 "My Name")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-create 'editable-field
                 :format "Address: %v"
                 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set widget-example-repeat
                                             '("En" "To" "Tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
    " for more information.\n\nNumbers: count to three below\n")
  (setq widget-example-repeat
        (widget-create 'editable-list
                       :entry-format "%i %d %v"
                       :notify
                       (lambda (widget &rest ignore)
                         (let ((old (widget-get widget
                                                ':example-length))
                               (new (length (widget-value widget))))
                           (unless (eq old new)
                             (widget-put widget ':example-length new)
                             (message "You can count to %d." new))))
                       :value '("One" "Eh, two?" "Five!")
                       '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "Tickle"))
                 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (widget &rest ignore)
                           (message "You selected %s"
                                    (widget-value widget)))
                 '(item "One") '(item "Another One.")
                 '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length
                                   (widget-value widget-example-repeat))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "Reset Form")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'ir-on-hold)
;;; ir-on-hold.el ends here
