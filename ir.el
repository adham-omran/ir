;;; ir.el --- Incremental Reading in Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/adham/ir
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'pdf-tools)
(require 'pdf-annot)


;; variables

(setq ir--db-location "/home/adham/Dropbox/org/tmp/ir.db")

(setq ir--extracts-location "/home/adham/Dropbox/org/tmp/testing-ir.org")

;; Database creation
(defvar ir-db (emacsql-sqlite ir--db-location))

(emacsql ir-db [:create-table ir
             ([(id text :primary-key) (afactor float)])])

;; Insert some data:
(emacsql ir-db [:insert :into ir
             :values (
                      ["c68d0b2b-992f-4c62-bbcd-a57efaff0cae" 2.0]
                      )])

(setq tmp-query (emacsql ir-db [:select [id]
                                    :from ir
                                    :where (> afactor 1.0)]))

;; functions

(defun ir--get-id-from-result (query)
  "Get dirty id from QUERY and clean it."
  (substring (substring (format "%s" query) 2 -2)))

(defun ir--go-to-extract (id)
  "Open a buffer with the extract of ID."
  (org-id-open id))

(ir--go-to-extract (ir--get-id-from-result tmp-query))

(defun ir-extract-pdf-tools ()
  "Create an extract from selection."
  (interactive)
  (ir--pdf-view-copy)
  (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region) "sky blue")
  ; Move to the extracts file
  (org-open-file ir--extracts-location)
  (goto-char (point-max))
  (insert "\n") ; for safety
  (insert "* " (format "%s" (current-time)) "\n")
  (yank)
  (org-id-get-create)
  (org-narrow-to-subtree))

(defun ir--pdf-view-copy ()
  "Copy the region to the `kill-ring'."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text)))
    (kill-new (mapconcat 'identity txt "\n"))))

(provide 'ir)
;;; ir.el ends here
