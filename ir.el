;;; ir.el --- Incremental Reading in Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 1.0.0
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

(defun ir-extract-pdf-tools ()
  "Create an extract from selection."
  (interactive)
  (ir--pdf-view-copy)
  (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region) "sky blue")
  ;; Move to the extracts file
  (ir--create-heading)
  (yank)
  ;; Add extract to the database
  (ir--insert-item (org-id-get) "text"))

                                        ; Read Functions
(defun ir-read-start ()
  "Start the reading session."
  (interactive)
  "Read the next item in the queue."
  (ir--open-item (ir--query-closest-time)))

(defun ir-read-next ()
  "Move to the next item in the queue."
  (interactive)
  (ir--compute-new-interval)
  (ir-read-start))

(defun ir-read-finish ()
  "Finish the reading sesssion."
  (interactive)
  (ir--compute-new-interval))



                                        ; Navigation Function
;; TODO Find pdf. Query in db for path.
;; TODO Find heading of open pdf. Use the full path to compare against db.


                                        ; Editing Functions
;; TODO Create (ir-change-priority id)


                                        ; View & Open Functions
;; TODO View all <type> function.
(defun ir--list-type (&optional type)
  "Retrun a list of items with a type. TYPE optional."
  (if (eq type nil)
      (progn
        (let ((type (completing-read "Choose type: " ir--list-of-unique-types)))
          (emacsql ir-db
                   [:select *
                    :from ir
                    :where (= type $s1)]
                   type)))
    (progn
        (emacsql ir-db
                 [:select *
                  :from ir
                  :where (= type $s1)]
                 type))))

(defun ir--list-unique-types ()
  "Return a list of every unique type."
  (emacsql ir-db
           [:select :distinct [type]
            :from ir]))

(defun ir--list-paths-of-type (list)
  "Return the nth element in a list of lists (LIST)."
  (let (result)
  (dolist (item list result)
    (push (nth 6 item) result))))

(defun ir-open-pdf ()
  "Open a pdf from those in the `ir-db'."
  (interactive)
  (ir--list-paths-of-type (ir--list-type "pdf"))
  (let ((file (completing-read "Choose pdf: " (ir--list-paths-of-type (ir--list-type "pdf")))))
    (find-file file)))



;; TODO Function to select a pdf from the database and open that pdf.


(provide 'ir)
;;; ir.el ends here
