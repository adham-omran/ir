;;; ir.el --- Incremental Reading -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 0.3.0
;; Keywords: wp, incremental reading
;; Homepage: https://github.com/adham-omran/ir
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package provides the features of Incremental Reading inside the Emacs
;; ecosystem.  Enabling one to process thousands of articles and books.
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

(defcustom ir-extracts-file "~/org/ir.org"
  "Location of the extracts."
  :type '(string))

(defvar ir--list-of-unique-types '()
  "List of unique values. Used for selecting a view.")

(defvar ir--p-column-names '(id 0 afactor 1 interval 2 priority 3 date 4
                                type 5 path 6))

;; Database creation
(defvar ir-db (emacsql-sqlite ir-db-location))


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
  "Create heading with an org-id."
  (org-open-file ir-extracts-file)
  (widen)
  (goto-char (point-max))
  (insert "\n") ; For safety
  (insert "* ")
  ;; TODO Better heading name.
  (insert (format "%s" (current-time)) "\n")
  (org-id-get-create)
  (org-narrow-to-subtree))

(defun ir--create-subheading ()
  "Create subheading with an org-id."
  (org-open-file ir-extracts-file)
  ;; (goto-char (point-max))
  ;; (insert "\n") ; for safety
  (org-insert-subheading 1)
  ;; TODO Better heading name.
  (insert (format "%s" (current-time)) "\n")
  (org-id-get-create)
  (org-narrow-to-subtree))

                                        ; Import Functions
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

(defun ir-add-web-article ()
  "Add URL of a web article to the database."
  (interactive)
  (let ((url (read-string "URL: ")))
    (ir--create-heading)
    (ir--insert-item (org-id-get) "web" url)))

;; TODO Import from Zotero
;;
;; This would greatly enhance the ability to add PDFs. It'd also seal the deal
;; for complete path as the file name.


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
        (nth 0 (emacsql ir-db
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

                                        ; Extract Functions
                                        ; From org

;; This works by taking the portion in the region and creating a new org-id
;; heading.
;;
;;; Cases
;; 1. We're in the `ir-extracts-file' file which implies the new extracts is a
;; child of the current extract.
;;
;; 2. We're not in the `ir-extracts-file'. I could use the same path mechanism I
;; use for PDFs. Create a new org-id heading per file and move to it when
;; creating an extract from the same file.
;;
;; Edge Cases to (2)
;;
;; 1. The user does not want to have extracts in another location.
;;
;; 2. The user is using org-roam.
;;
;; TODO Create `ir--extracts-file-p' as a predicate function to check if we're
;; in the extracts location.

;; TODO Why /not/ make this a general function?
;; Check if pdf and perform `ir--extract-pdf-tools'?

;; If the file is not a pdf. Clip the selection into the kill ring. Move into
;; an org-id heading. Create a subheading and paste.
(defun ir-extract-region ()
  "Extract from the current active region into appropriate org-id heading."
  (interactive)
  (when (equal (file-name-extension (buffer-file-name)) "pdf")
    (ir--extract-pdf-tools))
  (catch 'no-region
    (unless (use-region-p)
      (throw 'no-region
             (message "No active region.")))

    (kill-ring-save (region-beginning) (region-end))
    (ir--highlights-add-highlight)
    (deactivate-mark)
    ;; Check if we're in `ir-extracts-file'.
    (if (ir--extracts-location-p)
        (progn
          (org-insert-subheading nil)
          ;; TODO Better heading name.
          (insert (format "%s" (current-time)) "\n")
          (org-id-get-create)
          (yank)
          (org-narrow-to-subtree))
      (progn
        (ir--create-heading)
        (yank)))))

(defun ir--extracts-location-p ()
  "Check if we're in the correct location. BUFFER."
  (equal (buffer-file-name) ir-extracts-file))

                                        ; From pdf-tools

(defun ir--pdf-view-copy ()
  "Copy the region to the `kill-ring'."
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text)))
    (kill-new (mapconcat 'identity txt "\n"))))

(defun ir--extract-pdf-tools ()
  "Create an extract from selection."
  (ir--pdf-view-copy)
  (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region) "sky blue")
  ;; Move to the pdf file's heading
  (ir-navigate-to-heading)
  (ir--create-subheading)
  (yank)
  ;; Add extract to the database
  (ir--insert-item (org-id-get) "text"))

                                        ; Read Functions
;; (Minor mode) ideas
;;; A session starts a new emcas frame (alike org-noter).

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



                                        ; Navigation Functions
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
;; I think what I want to have is a find by each column. And a function that
;; moves to the heading given an ID.
;;

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
;; TODO Use (decode-time (seconds-to-time N))  to get human readable times.
(defun ir-view-items-by-date ()
  "View all items by their due date."
  (interactive)
  (let ((lists (emacsql ir-db [
                               :select *
                               :from ir
                               :order-by date
                               ])))
    ;; (find-file "/home/adham/Dropbox/org/tmp/view-test.org")
    (with-current-buffer (get-buffer-create "ir-view")
      (erase-buffer)
      (ir--view-create-table '("ID" "AF" "REP" "PR" "DATE" "TYPE" "PATH") lists))
    (switch-to-buffer "ir-view")
    (org-mode)))

(defun ir--view-create-table  (list-of-columns lists)
  "Generate an org-table from sql query using a LIST-OF-COLUMNS and LISTS."
  ;; Example use (create '("id" "date") '(("123" "23123") ("321" "23123123"))
  ;; Use list of columns to generate the head
  (insert "\n")
  (org-table-create (format "%sx%s" (length list-of-columns) (length lists)))
  (dolist (col-name list-of-columns)
    (org-table-next-field)
    (insert col-name))

  (dolist (row lists)
    (dolist (entry row)
      (org-table-next-field)
      (insert (format "%s" entry))))
  (org-table-next-field)
  (org-table-align))

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

(setq ir--list-of-unique-types (ir--list-unique-types))

(defun ir--list-paths-of-type (list)
  "Return the nth element in a list of lists (LIST)."
  (let (result)
    (dolist (item list result)
      (push (nth 6 item) result))))

(defun ir-open-pdf ()
  "Open a pdf from those in the `ir-db'."
  (interactive)
  (let ((file (completing-read "Choose pdf: " (ir--list-paths-of-type (ir--list-type "pdf")))))
    (find-file file)))

(defun ir-open-web ()
  "Open a web article from those in the `ir-db'."
  (interactive)
  (let ((file (completing-read "Choose URL: " (ir--list-paths-of-type (ir--list-type "web")))))
    (browse-url file)))

                                        ; Highlighting Functions

(defcustom ir-highlights-file "/home/adham/Dropbox/code/projects/ir/ir-highlights.el"
  "File to store highlights."
  :type '(string))

(defvar ir--highlights-saved (make-hash-table :test 'equal))

(defun ir--highlights-export ()
  "Exports highlist alist to file."
  (with-temp-file ir-highlights-file
    (delete-file ir-highlights-file)
    (insert (format "(setq %s '%S)\n" 'ir--highlights-saved (symbol-value 'ir--highlights-saved))))
  (load ir-highlights-file))

(defun ir--highlights-add-highlight ()
  "Add region to the saved highlight hashtable."
  ;; TODO Add the ability to highlight one word.
  (let (
        (old-list (gethash (org-id-get) ir--highlights-saved))
        (region (buffer-substring-no-properties (mark) (point))))
    (puthash (org-id-get) (cons region old-list) ir--highlights-saved)))

(defun ir--highlights-load ()
  "Load the highlight text for the current org-id."
  (dolist
      (i (gethash (org-id-get) ir--highlights-saved))
    (highlight-phrase i 'hi-blue)))

;; How to handle loading and exporting?
;;; Simply load highlights for every function that vists a heading. And export
;;; after every function that highlights.

                                        ; Collection Functions
;; These funcitons collect text from the web. They are most useful when configured with a DE/WM.

;; TODO ir-collect-from-web-selection

;; TODO ir-collect-from-clipboard: Insert \n then insert clipboard.

(provide 'ir)
;;; ir.el ends here
