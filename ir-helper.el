;;; ir-helper.el --- Help in working with the db -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 23, 2022
;; Modified: June 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/adham/ir-db-helper-functions
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;; (setq ir--table-list '("ir"))

(defun ir-helper-select-all (table)
  "Select all rows from TABLE."
  (interactive (list (completing-read "Choose: " "ir")))
  (message "%s" (emacsql ir-db [:select *
                                :from $r1]
                         table)))

(defun ir-helper-drop-table (table)
  "Drop TABLE."
  (interactive (list (completing-read "Choose: " "ir")))
  (emacsql ir-db [:drop :table $r1]
           table))

(defun ir-helper-find-item-from-id ()
  "Find the item in the db based on the org-id in context."
  (interactive)
  (message "%s" (ir--query-by-column (org-id-get) 'id t)))


(provide 'ir-db-helper-functions)
;;; ir-db-helper-functions.el ends here
