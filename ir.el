;;; ir.el --- Incremental Reading -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 01, 2026
;; Version: 0.13.0
;; Keywords: wp, incremental reading
;; Homepage: https://github.com/adham-omran/ir
;; Package-Requires: ((emacs "29.1") (org-roam "2.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Incremental Reading over Org material.  Items are Org headings (plain Org or
;; Org-roam nodes), keyed by `org-id'.  A SQLite database (native `sqlite.el')
;; holds only the schedule; the content lives in your Org files.
;;
;; Reviewing follows a simplified SuperMemo topic algorithm: the next interval is
;; the real elapsed interval times an A-Factor, and the queue is a priority queue
;; (due-ness gates eligibility, priority orders within it).
;;
;; Extraction promotes the selected region into a scheduled child heading in
;; place (a copy; the parent text is retained).
;;
;;; Code:
(require 'org)
(require 'org-id)
(require 'sqlite)
(require 'cl-lib)

(declare-function org-roam-node-read "ext:org-roam")
(declare-function org-roam-node-id "ext:org-roam")
(declare-function org-roam-node-list "ext:org-roam")
(declare-function org-roam-node-from-id "ext:org-roam")
(declare-function org-roam-node-title "ext:org-roam")
(defvar org-roam-directory)

(defgroup ir nil
  "Settings for `ir.el'."
  :link '(url-link "https://github.com/adham-omran/ir")
  :group 'convenience)

(defcustom ir-db-location "~/org/ir.db"
  "Location of the schedule database."
  :type 'string)

(defcustom ir-session-in-new-frame nil
  "If non-nil, `ir-start-session' opens a dedicated fullscreen frame."
  :type 'boolean)

(defcustom ir-afactor-increment 0.015
  "Amount the A-Factor grows on each review.
Higher values make review intervals lengthen faster."
  :type 'number)

;; --- Datastore (native sqlite.el) -------------------------------------------

(defconst ir--columns "id, afactor, interval, priority, due, last_reviewed"
  "Selected columns of the `ir' table, in schema order.
The order MUST match `ir--row->item'.")

(defconst ir--schema
  "CREATE TABLE ir (\
 id TEXT PRIMARY KEY,\
 afactor REAL NOT NULL DEFAULT 1.2,\
 interval INTEGER NOT NULL DEFAULT 1,\
 priority REAL NOT NULL DEFAULT 50.0,\
 due INTEGER NOT NULL,\
 last_reviewed INTEGER NOT NULL)"
  "DDL for the v0.13 `ir' table.")

(defvar ir--db nil
  "Memoized SQLite connection, or nil until first use.")

(defun ir--now ()
  "Return the current time as integer Unix seconds."
  (round (float-time)))

(defun ir--table-columns (db table)
  "Return TABLE's column-name symbols in DB, or nil when TABLE is absent."
  (mapcar (lambda (row) (intern (nth 1 row)))
          (sqlite-select db (format "PRAGMA table_info(%s)" table))))

(defun ir--ensure-schema (db)
  "Create the `ir' table in DB, or migrate a legacy one.
Invariant: on return, table `ir' has the v0.13 columns."
  (let ((cols (ir--table-columns db "ir")))
    (cond ((null cols) (sqlite-execute db ir--schema))
          ((or (memq 'type cols) (not (memq 'due cols)))
           (ir--migrate-from-old db)))))

(defun ir--migrate-from-old (db)
  "Rebuild the legacy {…,date,type,path} table in DB into the v0.13 schema.
Precondition: table `ir' exists with a `date' column.
Postcondition: table `ir' has the v0.13 columns, rows preserved, and
`last_reviewed' reconstructed as date minus interval days; a .bak is written."
  (copy-file (expand-file-name ir-db-location)
             (concat (expand-file-name ir-db-location) ".bak") t)
  (with-sqlite-transaction db
    (sqlite-execute db "ALTER TABLE ir RENAME TO ir_old")
    (sqlite-execute db ir--schema)
    (sqlite-execute db
                    "INSERT INTO ir (id, afactor, interval, priority, due, last_reviewed)\
 SELECT id, afactor, interval, priority, date, MAX(0, date - interval * 86400)\
 FROM ir_old")
    (sqlite-execute db "DROP TABLE ir_old")))

(defun ir--db ()
  "Return the live SQLite connection, opening and initializing it on first use.
Postcondition: a connection whose `ir' table matches the v0.13 schema."
  (or ir--db
      (setq ir--db
            (let ((db (sqlite-open (expand-file-name ir-db-location))))
              (ir--ensure-schema db)
              db))))

(defun ir--row->item (row)
  "Decode a SELECT ROW (columns in `ir--columns' order) into a plist.
Precondition: ROW has six fields in schema order."
  (cl-loop for key in '(:id :afactor :interval :priority :due :last_reviewed)
           for val in row
           append (list key val)))

(defun ir--select (sql &optional values)
  "Run SELECT SQL with VALUES, returning a list of item plists."
  (mapcar #'ir--row->item (sqlite-select (ir--db) sql values)))

(defun ir--all-ids ()
  "Return the org-ids of every queued item, ordered by due date."
  (mapcar (lambda (it) (plist-get it :id))
          (ir--select (concat "SELECT " ir--columns " FROM ir ORDER BY due ASC"))))

(defun ir--item (id)
  "Return the item plist for org-id ID, or nil when ID is not queued."
  (car (ir--select (concat "SELECT " ir--columns " FROM ir WHERE id = ?")
                   (list id))))

(defun ir--enqueue (id)
  "Insert org-id ID as a new item with the default schedule.
Precondition: ID is a non-empty org-id string.
Postcondition: exactly one row for ID exists.
Return t when inserted, nil when ID was already queued."
  (unless (ir--item id)
    (let ((now (ir--now)))
      (sqlite-execute (ir--db)
                      "INSERT INTO ir (id, due, last_reviewed) VALUES (?, ?, ?)"
                      (list id now now))
      t)))

(defun ir--delete (id)
  "Delete the queue row for ID.
Postcondition: no row with ID remains; the Org heading is untouched."
  (sqlite-execute (ir--db) "DELETE FROM ir WHERE id = ?" (list id)))

(defconst ir--editable-columns '("afactor" "interval" "priority" "due")
  "Columns `ir-edit' may modify; the whitelist guarding interpolated SQL.")

(defun ir--update-column (id column value)
  "Set whitelisted COLUMN of item ID to VALUE.
Precondition: COLUMN is in `ir--editable-columns'; identifiers cannot be
parameterized in SQL, so an unlisted COLUMN is a caller bug and is refused."
  (unless (member column ir--editable-columns)
    (error "Refusing to update non-whitelisted column %s" column))
  (sqlite-execute (ir--db)
                  (format "UPDATE ir SET %s = ? WHERE id = ?" column)
                  (list value id)))

;; --- Scheduling algorithm (faithful SuperMemo topic) ------------------------

(defun ir--reschedule (id)
  "Advance ID's schedule by the real-elapsed-interval topic formula.
Precondition: ID is queued.
Postcondition: interval = max(1, round(elapsed-days * afactor)); afactor
grows by `ir-afactor-increment'; due = now + interval days; last_reviewed
= now."
  (let ((item (ir--item id)))
    (if (not item)
        (message "IR: %s is not in the queue" id)
      (let* ((now (ir--now))
             (afactor (plist-get item :afactor))
             (elapsed-days (max 1 (round (/ (- now (plist-get item :last_reviewed))
                                            86400.0))))
             (interval (max 1 (round (* elapsed-days afactor)))))
        (with-sqlite-transaction (ir--db)
          (sqlite-execute
           (ir--db)
           "UPDATE ir SET interval = ?, afactor = ?, due = ?, last_reviewed = ? WHERE id = ?"
           (list interval
                 (+ afactor ir-afactor-increment)
                 (+ now (* interval 86400))
                 now
                 id)))))))

(defun ir--id-at-point ()
  "Return the org-id of the Org entry at point, or nil outside an Org buffer.
Guards `org-id-get', which signals via `org-element-at-point' in non-Org
buffers under Org 9.7+."
  (and (derived-mode-p 'org-mode) (org-id-get)))

(defun ir--reschedule-current ()
  "Reschedule the queued item under point.
Caller bug (reported, not signalled) if point is not within a queued heading."
  (let ((id (ir--id-at-point)))
    (if (and id (ir--item id))
        (ir--reschedule id)
      (message "IR: point is not on a queued item"))))

;; --- Import: register an existing org-id ------------------------------------

;;;###autoload
(defun ir-add ()
  "Queue the Org heading at point for incremental reading.
Precondition: an Org buffer with point within a heading.
Postcondition: the heading has an org-id and exactly one queue row.
Declines via `user-error' outside an Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "IR: ir-add works only in Org buffers"))
  (let ((id (org-id-get-create)))
    (if (ir--enqueue id)
        (message "IR: queued %s" id)
      (message "IR: already queued"))))

;;;###autoload
(defun ir-add-roam-node ()
  "Queue an Org-roam node selected by completion.
Precondition: `org-roam' is installed.
Postcondition: the node's id has exactly one queue row."
  (interactive)
  (require 'org-roam)
  (let ((id (org-roam-node-id (org-roam-node-read))))
    (if (ir--enqueue id)
        (message "IR: queued node %s" id)
      (message "IR: node already queued"))))

;;;###autoload
(defun ir-add-roam-directory ()
  "Queue every Org-roam node for incremental reading.
Precondition: `org-roam' is installed and `org-roam-directory' names a
directory.
Postcondition: every node in the Org-roam cache has a queue row; existing
rows are left intact.  The cache reflects the last `org-roam-db-sync', so
files not yet synced are not seen.
Caller declines via `user-error' when unconfigured, or aborts at the prompt."
  (interactive)
  (require 'org-roam)
  (unless (and (boundp 'org-roam-directory)
               (stringp org-roam-directory)
               (file-directory-p org-roam-directory))
    (user-error "IR: no org-roam-directory configured"))
  (when (yes-or-no-p (format "IR: queue all roam nodes under %s? " org-roam-directory))
    (let ((added 0))
      (dolist (node (org-roam-node-list))
        (when (ir--enqueue (org-roam-node-id node))
          (setq added (1+ added))))
      (message "IR: queued %d new roam node(s)" added))))

;; --- Extraction: promote a region in place ----------------------------------

(defun ir--extract-title (text)
  "Return a heading title from TEXT: its first non-blank line, at most 60 columns."
  (let ((line (car (split-string (string-trim text) "\n" t))))
    (truncate-string-to-width (or line "Extract") 60)))

(defun ir--extract-make-child (text)
  "Append a child heading containing TEXT to the heading at point.
Precondition: point is within an Org heading.
Postcondition: a new last child of that heading exists with an org-id and
TEXT as its body; return the child's org-id."
  (org-back-to-heading t)
  (let ((child-stars (make-string (1+ (org-current-level)) ?*))
        (title (ir--extract-title text)))
    (org-end-of-subtree t)
    (unless (bolp) (insert "\n"))
    (insert child-stars " " title "\n")
    (forward-line -1)
    (org-id-get-create)
    (org-end-of-meta-data t)
    (insert text "\n")
    (org-back-to-heading t)
    (org-id-get)))

;;;###autoload
(defun ir-extract-region ()
  "Extract the active region into a scheduled child heading, copied in place.
Precondition: an Org buffer with an active region under a heading.
Postcondition: a new child subheading holds a copy of the region, the
parent text is unchanged, the child is queued, and point returns to the
region start.
Declines (no mutation) on a non-Org buffer, no region, or no enclosing
heading."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "IR: extraction works only in Org buffers"))
  (unless (use-region-p)
    (user-error "IR: no active region to extract"))
  (let ((start (region-beginning))
        (text (buffer-substring-no-properties (region-beginning) (region-end))))
    (save-excursion
      (unless (ignore-errors (org-back-to-heading t) t)
        (user-error "IR: place the region under a heading"))
      (let ((id (ir--extract-make-child text)))
        (ir--enqueue id)
        (message "IR: extracted under %s" id)))
    (goto-char start)
    (deactivate-mark)))

;; --- Review session ---------------------------------------------------------

(defun ir--query-due ()
  "Return the most-due, highest-priority item, or nil when nothing is due.
Postcondition: an item plist with due <= now, ordered by priority then due."
  (car (ir--select
        (concat "SELECT " ir--columns
                " FROM ir WHERE due <= ? ORDER BY priority ASC, due ASC LIMIT 1")
        (list (ir--now)))))

(defun ir--narrow-to-item ()
  "Reveal the item at point: its subtree for a heading, else the whole file.
Precondition: called in an Org buffer at the item's location.
Postcondition: a file-level node (point before the first heading) is shown
widened; a heading is narrowed to its subtree."
  (widen)
  (unless (org-before-first-heading-p)
    (org-narrow-to-subtree)))

(defun ir--reading-setup (item)
  "Open ITEM's heading for review, narrowed and alone in the frame.
Precondition: ITEM is an item plist.
Loads `org-roam' when available so its `org-id-find' advice resolves roam-node
ids from the roam database.
Return `ok' after a successful open (subtree for a heading, whole file for a
file-level node), else `unresolved' when `org-id-open' cannot resolve the id.
Presentation never yields `unresolved'; this function never deletes."
  (require 'org-roam nil t)
  (if (condition-case nil
          (progn (org-id-open (plist-get item :id) nil) t)
        (error nil))
      (progn
        (delete-other-windows)
        (ir--narrow-to-item)
        'ok)
    'unresolved))

(defun ir--open-next ()
  "Open the next due item.
When a due item's heading cannot be opened (e.g. the note was deleted), offer to
delete its stale queue row and continue; declining stops the session."
  (let ((item (ir--query-due)))
    (cond
     ((null item) (message "IR: queue empty for today"))
     ((eq (ir--reading-setup item) 'ok) item)
     ((yes-or-no-p (format "IR: cannot open %s -- delete its stale queue row? "
                           (plist-get item :id)))
      (ir--delete (plist-get item :id))
      (ir--open-next))
     (t (message "IR: stopped at unresolvable item %s" (plist-get item :id))))))

;;;###autoload
(defun ir-start-session ()
  "Begin a review session at the most-due item.
Postcondition: with `ir-session-in-new-frame', a fullscreen frame named
\"ir-session\" is created first."
  (interactive)
  (when ir-session-in-new-frame
    (make-frame '((name . "ir-session")))
    (select-frame-by-name "ir-session")
    (toggle-frame-fullscreen))
  (ir--open-next))

;;;###autoload
(defun ir-read-next ()
  "Reschedule the item under review, then open the next due one.
Precondition: point is within the heading of the item under review."
  (interactive)
  (ir--reschedule-current)
  (ir--open-next))

;;;###autoload
(defun ir-end-session ()
  "Reschedule the item under review and end the session."
  (interactive)
  (ir--reschedule-current)
  (when ir-session-in-new-frame
    (delete-frame)))

;;;###autoload
(defun ir-validate ()
  "Report queued ids whose Org heading no longer exists; mutate nothing."
  (interactive)
  (let ((orphans (cl-remove-if #'org-id-find-id-file (ir--all-ids))))
    (message "IR: %d orphan(s)%s" (length orphans)
             (if orphans (format ": %S" orphans) ""))))

;; --- Navigation -------------------------------------------------------------

;;;###autoload
(defun ir-navigate-to-heading (&optional id)
  "Jump to the Org heading for ID, widened then narrowed.
ID defaults to the org-id at point; messages and does nothing when none is
available.  Precondition: ID resolves to an existing heading or file node."
  (interactive)
  (let ((id (or id (ir--id-at-point))))
    (if id
        (progn (org-id-open id nil) (ir--narrow-to-item))
      (message "IR: no org-id at point"))))

;; --- View & maintenance -----------------------------------------------------

(defun ir--format-time (n)
  "Render integer Unix-seconds N as an ISO date string."
  (format-time-string "%F" n))

(defun ir--id-title (id)
  "Return a display label for ID without visiting any file.
Uses the Org-roam database title when available, otherwise ID itself."
  (or (and (require 'org-roam nil t)
           (ignore-errors (org-roam-node-title (org-roam-node-from-id id))))
      id))

(defun ir--read-id (prompt)
  "Read a queued item id via PROMPT, completing over \"title -- id\" labels."
  (let ((alist (mapcar (lambda (id) (cons (format "%s -- %s" (ir--id-title id) id) id))
                       (ir--all-ids))))
    (cdr (assoc (completing-read prompt alist nil t) alist))))

;;;###autoload
(defun ir-view ()
  "Show all queued items as an Org table ordered by due date."
  (interactive)
  (let ((items (ir--select
                (concat "SELECT " ir--columns " FROM ir ORDER BY due ASC"))))
    (pop-to-buffer (get-buffer-create "*ir-view*"))
    (erase-buffer)
    (org-mode)
    (insert "| ID | AF | Interval | Priority | Due |\n|-\n")
    (dolist (it items)
      (insert (format "| %s | %.3f | %d | %.1f | %s |\n"
                      (plist-get it :id)
                      (plist-get it :afactor)
                      (plist-get it :interval)
                      (plist-get it :priority)
                      (ir--format-time (plist-get it :due)))))
    (goto-char (point-min))
    (org-table-align)))

;;;###autoload
(defun ir-edit ()
  "Edit one scheduling column of a queued item chosen by completion.
The id column is immutable; editing `interval' reschedules `due' to now
plus that many days, and editing `due' sets the next date directly."
  (interactive)
  (let ((id (ir--read-id "Edit item: "))
        (column (completing-read "Column: " ir--editable-columns nil t)))
    (pcase column
      ("due"
       (ir--update-column id "due"
                          (time-convert (org-read-date nil t nil "New due: ")
                                        'integer)))
      ("interval"
       (let ((days (read-number "Interval (days): ")))
         (ir--update-column id "interval" days)
         (ir--update-column id "due" (+ (ir--now) (* days 86400)))))
      ((or "afactor" "priority")
       (ir--update-column id column (read-number (format "New %s: " column)))))
    (message "IR: updated %s of %s" column id)))

;;;###autoload
(defun ir-delete ()
  "Delete a queued item's row chosen by completion; leave its heading intact."
  (interactive)
  (let ((id (ir--read-id "Delete item: ")))
    (ir--delete id)
    (message "IR: deleted %s" id)))

;;;###autoload
(defun ir-open ()
  "Open a queued item chosen by title, narrowed for reading."
  (interactive)
  (let ((id (ir--read-id "Open item: ")))
    (unless (eq (ir--reading-setup (ir--item id)) 'ok)
      (message "IR: cannot open %s (heading missing or id not indexed)" id))))

;;;###autoload
(defun ir-find-item-at-point ()
  "Echo the queue row for the org-id of the heading at point."
  (interactive)
  (let ((id (ir--id-at-point)))
    (message "%S" (and id (ir--item id)))))

(provide 'ir)
;;; ir.el ends here
