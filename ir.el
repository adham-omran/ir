;;; ir.el --- Incremental Reading -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 0.7.0
;; Keywords: wp, incremental reading
;; Homepage: https://github.com/adham-omran/ir
;; Package-Requires: ((emacs "24.4"))
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
(require 'citar)
(require 'org-roam)

;; Load
(load "~/Dropbox/code/projects/ir/ir-helper.el")

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

(defcustom ir-highlights-file "~/org/ir-highlights.el"
  "File to store highlights."
  :type '(string))

(defcustom ir-return-to-pdf t
  "If t return to the PDF after extracting."
  :type '(boolean))

(defvar ir--list-of-unique-types '()
  "List of unique values. Used for selecting a view.")

(defvar ir--p-column-names '(id 0 afactor 1 interval 2 priority 3 date 4
                                type 5 path 6))


;; Database creation
(defvar ir-db (emacsql-sqlite ir-db-location))


(emacsql ir-db [:create-table :if-not-exists ir
                ([(id text :primary-key)
                  (afactor real :default 1.2)
                  (interval integer :default 1)
                  (priority real :default 50.0)
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
  (org-insert-subheading 1)
  ;; TODO Better heading name.
  (insert (format "%s" (current-time)) "\n")
  (org-id-get-create)
  (org-narrow-to-subtree))

(defun ir--check-duplicate (column value)
  "Check in COLUMN for VALUE."
  (emacsql ir-db
           [:select *
            :from ir
            :where (= $i1 $s2)]
           column
           value))

                                        ; Import Functions
                                        ; PDF
(defun ir-add-pdf (path)
  "Select and add a PATH PDF file to the database."
  (interactive (list (read-file-name "Select PDF to add: " nil nil t)))
  ;; First check if the file is a PDF. Second check if the file has already been
  ;; added.
  ;; (setq path '("~/Dropbox/org/tmp/lorem-ipsum.pdf"))
  ;; (setq path (expand-file-name path))

  (if (equal (file-name-extension path) "pdf")
      (if (ir--check-duplicate 'path (expand-file-name path))
          (message "%s.pdf is already in the database." (file-name-base path))
        (progn
          (ir--create-heading)
          (ir--insert-item (org-id-get) "pdf" path))
        (find-file path))
    (message "File %s is not a PDF file." path)))


                                        ; Web

(defun ir-add-web-article ()
  "Add URL of a web article to the database."
  (interactive)
  (let ((url (read-string "URL: ")))
    (ir--create-heading)
    (ir--insert-item (org-id-get) "web" url)))

                                        ; Bibtex

(defun ir-add-bibtex-entry ()
  "Select an entry from bibliography, if there's a file, insert into db."
  (interactive)
  (let ((ref (citar-select-ref)))
    ;; (message "%s" (assoc "file" ref))

    (cond ((equal (assoc "has-file" ref) nil) (message "No file."))
          ((ir--check-duplicate 'path (cdr (assoc "file" ref)))
           (message "%s.pdf already exists." (file-name-base (cdr (assoc "file" ref)))))
          (t
           (progn
             (ir--create-heading)
             (ir--insert-item (org-id-get) "pdf" (cdr (assoc "file" ref)))
             (find-file (cdr (assoc "file" ref))))))))

                                        ; org-roam
(defun ir-add-current-roam-node ()
  "Add the currently visited roam node."
  (interactive)
  (ir--insert-item (org-id-get) "text"))

(cl-defun ir-add-roam-node-by-find (&optional initial-input filter-fn pred)
  "Find and open an Org-roam node by its title or alias. Then add it.

No clue what INITIAL-INPUT, FILTER-FN or PRED do."
  (interactive current-prefix-arg)
  (let ((node (org-roam-node-read initial-input filter-fn pred)))
    (cond ((ir--check-duplicate 'id (org-roam-node-id node)) (message "Node already exists."))
          (t (ir--insert-item (org-roam-node-id node) "txt")))))

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
      (find-file item-path))
    (when (equal item-type "web")
      (browse-url item-path)
      (ir-navigate-to-heading item-id)
      (message "Open URL complete."))))

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
  ;; The way I have it compute new interval for a PDF file is as follows.
  ;;
  ;; Navigate to the header of the PDF file. Use its ID to update the PDF's
  ;; interval. This makes sense because the PDF is just another ID in the db.
  (if (equal (file-name-extension (buffer-file-name)) "pdf")
      (ir-navigate-to-heading))
  (let (
        (item (ir--query-by-column (org-id-get) 'id t)))
    (let (
          (old-a (ir--return-column 'afactor item))
          (old-interval (ir--return-column 'interval item))
          (old-date (ir--return-column 'date item)))
      (ir--update-value (org-id-get) "interval" (round (* old-interval old-a)))
      (ir--update-value (org-id-get) "afactor" (+ old-a 0.015))
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
;; If the file is not a PDF. Clip the selection into the kill ring. Move into

;; an org-id heading. Create a subheading and paste.
(defun ir-extract-region ()
  "Extract from the current active region into appropriate org-id heading."
  (interactive)
  (when (equal (file-name-extension (buffer-file-name)) "pdf")
    (ir--extract-pdf-tools)
    (when ir-return-to-pdf (previous-buffer)))
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

(defun ir--extract-pdf-tools ()
  "Create an extract from selection."
  (ir--pdf-view-copy)
  (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region) "sky blue")
  ;; Move to the PDF file's heading
  (ir-navigate-to-heading)
  (ir--create-subheading)
  (yank)
  ;; Add extract to the database
  (ir--insert-item (org-id-get) "text"))

(defun ir--pdf-view-copy ()
  "Copy the region to the `kill-ring'."
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text)))
    (kill-new (mapconcat 'identity txt "\n"))))

                                        ; Read Functions

(defun ir-read-next ()
  "Move to the next item in the queue, compute next interval."
  (interactive)
  ;; TODO How to handle not finding an item.
  ;;
  ;; TODO Update to handle the session style. Perhaps I could check by input and
  ;; split the window if the type is pdf.
  (ir--reading-setup (ir--query-closest-time))
  ;; (ir--open-item (ir--query-closest-time))
  ;; (ir--compute-new-interval)
  )

(defun ir-start-session ()
  "Start a session."
  (interactive)
  (make-frame '((name . "ir-session")))
  (select-frame-by-name "ir-session")
  (toggle-frame-fullscreen)
  (ir--reading-setup (ir--query-closest-time)))

(defun ir-end-session ()
  "End a session."
  (interactive)
  (ir--compute-new-interval)
  (delete-frame))

(defun ir--reading-setup (list)
  "Prepare the ideal environmet given a LIST."
  (let ((item-id (nth 0 list))
        (item-type (nth 5 list))
        (item-path (nth 6 list)))
    (message "%s" item-path)
    ;; Body
    (when (equal item-type "pdf")
      (delete-other-windows)
      (find-file item-path)
      (split-window-horizontally)
      (ir-navigate-to-heading))

    (when (equal item-type "web")
      (toggle-frame-fullscreen)
      (browse-url item-path))

    (when (equal item-type "txt")
      (org-id-open item-id nil))))



                                        ; Navigation Functions
(defun ir-navigate-to-source ()
  "Navigate to the source of a heading if one exists."
  (interactive)
  ;; get the id, use that to get the type, use the path.
  (ir--open-item (ir--query-by-column (org-id-get) 'id t)))

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
                                  (format "%s" (buffer-file-name))
                                  'path t)))))
  (find-file (org-id-find-id-file id))
  (widen) ;; In case of narrowing by previous functions.
  (goto-char (cdr (org-id-find id)))
  (org-narrow-to-subtree))

                                        ; Editing Functions
(defun ir-edit-column ()
  "Search for an item."
  ;; TODO Date
  (interactive)
  (let (
        (lists (let (
                     (column (completing-read "What column do you want to search: "
                                              '("id" "afactor" "interval" "priority" "type" "path") nil t))
                     (search-me (completing-read "What to search for: " nil)))
                 ;; Body
                 (emacsql ir-db [:select *
                                 :from ir
                                 :where $i1 :like $s2
                                 ]
                          (intern column) ;; Turns a string into a symbol
                          (concat "%" search-me "%")))))
    ;; Find file approach
    (find-file (make-temp-file "ir-view" nil ".org"))
    (erase-buffer)

    (ir--view-create-table lists)
    ;; Update the value
    (let ((result (completing-read "Which result: " lists))
          (column-name (completing-read "What column do you want to edit? " '("id" "afactor" "interval" "date" "priority" "type" "path") nil t)))
      (ir--update-value result
                        column-name
      (cond ((member column-name '("id" "afactor" "interval" "priority" "date")) (read-number "New value: "))
            (t (read-string "New value: ")))))))

;; 1. Choose what column to search
;; 2. Enter search query
;; 3. Choose the column you want to edit
;; 4. Enter new value

                                        ; View & Open Functions
(defun ir-view-items-by-date ()
  "View all items by their due date."
  (interactive)
  (let ((lists (emacsql ir-db [
                               :select *
                               :from ir
                               :order-by date
                               ])))
    ;; Create a file
    (find-file (make-temp-file "ir-view" nil ".org"))
    (ir--view-create-table lists)
    (goto-char (point-max))
    (insert "#+tblfm: @<<$5..@$5='(ir--format-time (string-to-number $5))")))

(defun ir--format-time (N)
  "Used in a table to convert the N dates into human-readable times."
  (format-time-string "%F, %R:%S" N))

(defun ir--view-create-table (lists)
  "Transform a list of LISTS into a table."
  (insert "\n")
  (insert (format "%s" lists))
  (goto-char (point-min))
  (while (re-search-forward ") (" nil t)
    (replace-match "\n|"))
  (goto-char (point-min))
  (while (re-search-forward " \\([0-9]*.[0-9]*\\) \\([0-9]*.[0-9]*\\) \\([0-9]*.[0-9]*\\) \\([0-9]*.[0-9]*\\) \\([a-z]*\\) " nil t)
    (replace-match "|\\1|\\2|\\3|\\4|\\5|"))
  (goto-char (point-max))
  (backward-delete-char 2)
  (goto-char (point-min))
  (insert "|ID|AF|Interval|PR|DATE|TYPE|PATH\n")
  (delete-char 3)
  (insert "|")
  (org-table-align)
  (goto-char (point-min))
  (org-table-insert-hline))

(defun ir--list-type (&optional type)
  "Return a list of items with a type. TYPE optional."
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
  "Open a PDF from those in the `ir-db'."
  (interactive)
  (let ((file (completing-read "Choose PDF: " (ir--list-paths-of-type (ir--list-type "pdf")))))
    (find-file file)))

(defun ir-open-web ()
  "Open a web article from those in the `ir-db'."
  (interactive)
  (let ((file (completing-read "Choose URL: " (ir--list-paths-of-type (ir--list-type "web")))))
    (browse-url file)))

                                        ; Highlighting Functions

(defvar ir--highlights-saved (make-hash-table :test 'equal))

(defun ir--highlights-export ()
  "Exports highlights alist to file."
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
;;; Simply load highlights for every function that visits a heading. And export
;;; after every function that highlights.

                                        ; Collection Functions
                                        ;; web - firefox
;; These functions collect text from the web. They are most useful when configured with a DE/WM.

;; TODO org-protocol?

;; TODO ir-collect-from-web-selection

;; TODO ir-collect-from-clipboard: Insert \n then insert clipboard.

(provide 'ir)
;;; ir.el ends here
