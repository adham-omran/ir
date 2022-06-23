;;; ir-db-helper-functions.el --- Help in working with the db -*- lexical-binding: t; -*-
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

(setq ir--table-list '("ir_extracts" "ir_material_pdf"))

(defun ir-db-helper-select-all (table)
  "Select all rows from TABLE."
  (interactive (list (completing-read "Choose: " ir--table-list)))
  (message "%s" (emacsql ir-db [:select *
                                :from $r1]
                         table)))

(defun ir-db-helper-drop-table (table)
  "Drop TABLE."
  (interactive (list (completing-read "Choose: " ir--table-list)))
  (emacsql ir-db [:drop :table $r1]
           table))

(defun ir-db-helper-find-item-from-id ()
  (interactive)
  (message "%s" (nth 0 (ir--find-item (org-id-get)))))


(provide 'ir-db-helper-functions)
;;; ir-db-helper-functions.el ends here
