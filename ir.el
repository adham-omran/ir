;;; ir.el --- Incremental Reading -*- lexical-binding: t; -*-
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
;; This package provides the features of Incremental Reading inside the Emacs
;; ecosystem. It leverages utilities from org-mode, pdf-tools, emacsql,
;; anki-editor and ankifier.
;;
;;
;;; Code:
(require 'pdf-tools)
(require 'pdf-annot)
(require 'emacsql-sqlite)
(require 'org)
(require 'org-id)
(require 's)

;; Variables

(defgroup ir nil
  "Settings for `ir.el'."
  :link '(url-link "https://github.com/adham-omran/ir")
  :group 'convenience)

(defcustom ir-db-location "~/org/ir.db"
  "Location of the database."
  :type '(string))

(defcustom ir-extracts-location "~/org/ir.org"
  "Location of the extracts."
  :type '(string))

(defvar ir--list-of-unique-types (ir--list-unique-types)
  "List of unique values. Used for selecting a view.")

(defvar ir--p-column-names '(id 0 afactor 1 interval 2 priority 3 date 4
                            type 5 path 6))

;; Database creation
(defvar ir-db (emacsql-sqlite ir-db-location))


;; Design of database
;;
;; One of my goals is to support as many possible file types as possible. This
;; would allow one to incrementally learn any piece of material. This also
;; allows others to easily extend the program by including their favorite file
;; types and programs to open them.
;;
;; One way to query the database is to sort the `ir' table by date, then match
;; the file type to a function that opens that file type.
;;
;; Such files have a path which is inserted in the path column.
;;
;; Example
;; File type - Method - Description.
;; text - org-id-find - A simple org heading.
;; pdf - dired-find-file - A pdf file.
;; mp4 - TODO - A video.

(emacsql ir-db [:create-table :if-not-exists ir
                ([(id text :primary-key)
                  (afactor real :default 1.5)
                  (interval integer :default 1)
                  (priority integer :default 50)
                  (date integer)
                  (type text :not-null)
                  (path text)
                  ])])

; TODO Improve how headings are created.
(defun ir--create-heading ()
  "Create heading with org-id."
  (org-open-file ir-extracts-location)
  ;; (goto-char (point-max))
  ;; (insert "\n") ; for safety
  (org-insert-heading)
  ;; TODO Better heading name.
  (insert (format "%s" (current-time)) "\n")
  (org-id-get-create)
  (org-narrow-to-subtree))

(defun ir--create-subheading ()
  "Create heading with org-id."
  (org-open-file ir-extracts-location)
  ;; (goto-char (point-max))
  ;; (insert "\n") ; for safety
  (org-insert-subheading 1)
  ;; TODO Better heading name.
  (insert (format "%s" (current-time)) "\n")
  (org-id-get-create)
  (org-narrow-to-subtree))

                                        ; Material Import Functions
                                        ; Importing a PDF
(defun ir-add-pdf (path)
  "Select and add a PATH pdf file to the databse."
  (interactive (list (read-file-name "Select PDF to add: ")))
  ;; First check if the file is a pdf. Second check if the file has already been
  ;; added.
  (if (equal (file-name-extension path) "pdf")
      (if (ir--check-duplicate-path path)
          (message "File %s is already in the database." path)
        (progn
        (ir--create-heading)
        (ir--insert-item (org-id-get) "pdf" path))
        (find-file path))
    (message "File %s is not a pdf file." path)))


                                        ; Database Functions
(defun ir--open-item (list)
  "Opens an item given a LIST. Usually from a query."
  (let ((item-id (nth 0 list))
        (item-type (nth 5 list))
        (item-path (nth 6 list)))
    ;; Body
    (when (equal item-type "text")
      (ir-navigate-to-heading item-id))
    (when (equal item-type "pdf")
      (find-file item-path))))

(defun ir--query-closest-time ()
  "Query `ir-db' for the most due item.

The order is first by time from smallest number (closest date) to
largest number (farthest date)."
  ;; TODO Refactor to use ir-return.
  ;;
  ;; TODO Enable sorting by priority.
  (nth 0 (emacsql ir-db
                  [:select *
                   :from ir
                   :order-by date])))

(defun ir--query-by-column (value column &optional return-item)
  "Search for VALUE in COLUMN.

If RETURN-ITEM is non-nil, returns the first result. I have this
to avoid writing (nth 0) in all return functions that want a
single item to return the value of a column from."
  (if return-item
      (progn
        (nth 0(emacsql ir-db
                       [:select *
                        :from ir
                        :where (= $s1 $i2)]
                       value
                       column)))
    (progn
      (emacsql ir-db
               [:select *
                :from ir
                :where (= $s1 $i2)]
               value
               column))))

;; Upgrading query-for-path. Make it retun the intended column.

(defun ir--return-column (column query)
  "Using a plist, access any value from a QUERY search in COLUMN.
Prime use case it to get the id of a particular query. Note this
only access the first result."
  (nth (plist-get ir--p-column-names column) query))

(defun ir--check-duplicate-path (path)
  "Check `ir-db' for matching PATH."
  (emacsql ir-db
           [:select *
            :from ir
            :where (= path $s1)]
           path))

(defun ir--insert-item (id type &optional path)
  "Insert item into `ir' database with TYPE and ID."
  (unless path (setq path nil)) ;; Check if a path has been supplied.
  (emacsql ir-db [:insert :into ir [id date type path]
                  :values (
                           [$s1 $s2 $s3 $s4])]
           id
           (round (float-time))
           type
           path))

(defun ir--find-item (id)
  "Return a list given ID."
  (emacsql ir-db [:select *
                  :from ir
                  :where (= $s1 id)]
           id))

(defun ir--update-value (id column value)
  "Update the VALUE for the item ID with at COLUMN."
  (emacsql ir-db [:update ir
                  :set $r3 := $v1
                  :where (= $v2 id)]
           (list (vector value))
           (list (vector id))
           column))


                                        ; Algorithm Functions
(defun ir--compute-new-interval ()
  "Compute a new interval for the item of ID.
Part of the ir-read function."
  ;; The way I have it compute new interval for a pdf file is as follows.
  ;;
  ;; Navigate to the header of the pdf file. Use its ID to update the pdf's
  ;; interval. This makes sense because the PDF is just another ID in the db.
  (if (equal (file-name-extension (buffer-file-name)) "pdf")
      (ir-navigate-to-heading))
  (let (
      (item (ir--query-by-column (org-id-get) 'id t)))
  (let (
        (old-a (ir--return-column 'afactor item))
        (old-interval (ir--return-column 'interval item))
        (old-date (ir--return-column 'date item)))
    (ir--update-value (org-id-get) "interval" (round (* old-interval (+ old-a 0.08))))
    (ir--update-value (org-id-get) "afactor" (+ old-a 0.08))
    (ir--update-value (org-id-get) "date" (+ old-date (* 24 60 60 old-interval))))))

  ;; (let (
  ;;       (item (nth 0 (ir--find-item (org-id-get)))))
  ;;   (let (
  ;;         (id (nth 0 item))
  ;;         (old-a (nth 1 item))
  ;;         (old-interval (nth 2 item))
  ;;         (old-date (nth 3 item)))
  ;;     (ir--update-value id "interval" (round (* old-interval (+ old-a 0.08))))
  ;;     (ir--update-value id "afactor" (+ old-a 0.08))
  ;;     (ir--update-value id "date" (+ old-date (* 24 60 60 old-interval))))))

                                        ; Extract Functionality
                                        ; From pdf-tools

;; TODO Just extract but leave pdf
;;; TODO Children behavior. There are N possible ideas:
;; 1. Whenever an extract is made, move to the heading containing the id of the
;; pdf file. Create a subheading for each cloze.
;;
;; 2. Have no children.

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
  ;; Move to the pdf file's heading
  (ir-navigate-to-heading)
  (ir--create-subheading)
  (yank)
  ;; Add extract to the database
  (ir--insert-item (org-id-get) "text"))

                                        ; Read Functions
(defun ir-read-start ()
  "Start the reading session."
  (interactive)
  ;; TODO How to handle not finding an item.
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
;; TODO Find pdf. Query in db for path. To do this, I can check if the path of
;; the ID is non-nil, if it's non-nil, then I can navigate to that path with a
;; simple open operation.
(defun ir-navigate-to-source ()
  ;; (interactive)
  "Navigate to the source of a heading if one exists.")


;;
;; This exposed the problem of storing paths with ~ vs /home/USER/
;;
;; TODO Rework navigation functions.
;;
;; DONE Write an API to find stuff
;;
;; DONE Clearly define what an `item' and a `query' are.
;;
;; I think what I want to have is a find by each column. And a function that
;; moves to the heading given an ID.
;;
;; DONE Define what an ir-query function does.
;;
;; TODO Define all the ir-<subset> functions.

;; DONE Find heading of open pdf. Use the full path to compare against db.
(defun ir-navigate-to-heading (&optional id)
  "Navigate to the heading given ID."
  (interactive)
  (if (equal (file-name-extension (buffer-file-name)) "pdf")
      (progn
        (setq id
              (ir--return-column 'id ;; Uses the results of `'ir--query-by-column'
                                 ;; to return only the 'id value
               (ir--query-by-column ;; Results in an item of the form ("id"
                                    ;; afactor ... path)

                ;; TODO Figure out a regex that works. Or save all paths as
                ;; complete.
                (s-replace "/home/adham/" "~/" (format "%s" (buffer-file-name)))
                'path t)))))
  (find-file (org-id-find-id-file id))
  (widen) ;; In case of narrowing by previous functions.
  (goto-char (cdr (org-id-find id)))
  (org-narrow-to-subtree))

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

(provide 'ir)
;;; ir.el ends here
