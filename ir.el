;;; ir.el --- Incremental Reading -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Adham Omran
;;
;; Author: Adham Omran <adham.rasoul@gmail.com>
;; Maintainer: Adham Omran <adham.rasoul@gmail.com>
;; Created: June 22, 2022
;; Modified: June 02, 2026
;; Version: 0.15.0
;; Keywords: wp, incremental reading
;; Homepage: https://github.com/adham-omran/ir
;; Package-Requires: ((emacs "29.1") (org-roam "2.3") (gptel "0.9.0"))
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
(require 'gptel)

(declare-function org-roam-node-read "ext:org-roam")
(declare-function org-roam-node-id "ext:org-roam")
(declare-function org-roam-node-list "ext:org-roam")
(declare-function org-roam-node-from-id "ext:org-roam")
(declare-function org-roam-node-title "ext:org-roam")
(declare-function org-roam-node-file "ext:org-roam")
(declare-function org-roam-backlinks-get "ext:org-roam")
(declare-function org-roam-db-clear-file "ext:org-roam")
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

(defcustom ir-delete-to-trash t
  "If non-nil, `ir-done-and-delete' moves a deleted file to the OS trash."
  :type 'boolean)

(defcustom ir-done-log-file "~/org/ir-done.org"
  "File to which `ir-done-and-delete' appends a completion record.
Set to nil to disable logging."
  :type '(choice file (const nil)))

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

(defvar ir--current-id nil
  "Org-id of the item currently under review, or nil between sessions.
Set by `ir--open-next' when an item opens; read by `ir--reschedule-current' so a
review is recorded for the opened item regardless of where point drifts.")

(defvar ir--session-active nil
  "Non-nil while a review session is active; the gate for queue commands.
Set by `ir-start-session', cleared by `ir-end-session'.")

(defvar ir--session-count 0
  "Items read in the active session; reported by `ir-end-session'.")

(defvar ir--session-start-time nil
  "Unix seconds at which the active session started, or nil between sessions.")

(defun ir--reschedule-current ()
  "Reschedule the item under review and count it as read.
No-op when no item is under review (e.g. the queue drained mid-session)."
  (when (and ir--current-id (ir--item ir--current-id))
    (ir--reschedule ir--current-id)
    (cl-incf ir--session-count)))

(defun ir--require-session ()
  "Signal a `user-error' unless a review session is active.
Precondition of every command except `ir-start-session' and the importers."
  (unless ir--session-active
    (user-error "IR: not in a session (start one with `ir-start-session')")))

(defun ir--require-current ()
  "Signal a `user-error' unless a session is active with an item under review."
  (ir--require-session)
  (unless (and ir--current-id (ir--item ir--current-id))
    (user-error "IR: no item under review")))

(defun ir--format-elapsed (seconds)
  "Format integer SECONDS as an \"H:MM:SS\" string (hours unbounded, no wrap)."
  (format "%d:%02d:%02d"
          (/ seconds 3600) (% (/ seconds 60) 60) (% seconds 60)))

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
Precondition: a session is active, in an Org buffer with an active region under
a heading.
Postcondition: a new child subheading holds a copy of the region, the
parent text is unchanged, the child is queued, and point returns to the
region start.
Declines (no mutation) on a non-Org buffer, no region, or no enclosing
heading."
  (interactive)
  (ir--require-session)
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
  "Open the next due item and record it as the item under review.
When a due item's heading cannot be opened (e.g. the note was deleted), offer to
delete its stale queue row and continue; declining stops the session."
  (let ((item (ir--query-due)))
    (cond
     ((null item) (setq ir--current-id nil) (message "IR: queue empty for today"))
     ((eq (ir--reading-setup item) 'ok)
      (setq ir--current-id (plist-get item :id))
      item)
     ((yes-or-no-p (format "IR: cannot open %s -- delete its stale queue row? "
                           (plist-get item :id)))
      (ir--delete (plist-get item :id))
      (ir--open-next))
     (t (setq ir--current-id nil)
        (message "IR: stopped at unresolvable item %s" (plist-get item :id))))))

;;;###autoload
(defun ir-start-session ()
  "Begin a review session at the most-due item.
No-op with a message when a session is already active; ending it is the only
exit.  Postcondition: when an item opens, `ir--session-active' is t (and with
`ir-session-in-new-frame' a fullscreen frame \"ir-session\" exists); an empty
queue or a declined stale item leaves no session active."
  (interactive)
  (if ir--session-active
      (message "IR: already in a session")
    (when ir-session-in-new-frame
      (make-frame '((name . "ir-session")))
      (select-frame-by-name "ir-session")
      (toggle-frame-fullscreen))
    (setq ir--session-active t
          ir--session-count 0
          ir--session-start-time (ir--now))
    (ir--open-next)
    (unless ir--current-id
      (setq ir--session-active nil)
      (when ir-session-in-new-frame (delete-frame)))))

;;;###autoload
(defun ir-read-next ()
  "Reschedule the item under review, then open the next due one.
Precondition: a session is active with an item under review."
  (interactive)
  (ir--require-current)
  (ir--reschedule-current)
  (ir--open-next))

;;;###autoload
(defun ir-end-session ()
  "Conclude the active session: reschedule the item under review, then report.
Reports items read, elapsed time as H:MM:SS, and items still due, and resets all
session state.  Precondition: a session is active."
  (interactive)
  (ir--require-session)
  (ir--reschedule-current)
  (let ((read ir--session-count)
        (elapsed (ir--format-elapsed (- (ir--now) ir--session-start-time)))
        (due (caar (sqlite-select
                    (ir--db) "SELECT COUNT(*) FROM ir WHERE due <= ?"
                    (list (ir--now))))))
    (setq ir--session-active nil
          ir--current-id nil
          ir--session-count 0
          ir--session-start-time nil)
    (when ir-session-in-new-frame
      (delete-frame))
    (message "IR: session ended — %d items read in %s, %d still due"
             read elapsed due)))

;;;###autoload
(defun ir-set-priority ()
  "Set the priority of the item under review, clamped to [0,100].
Lower is sooner: 0 is highest priority, 100 is lowest.  Priority orders the due
queue only; it does not reschedule the item.
Precondition: a session is active with an item under review."
  (interactive)
  (ir--require-current)
  (let ((priority (max 0 (min 100 (read-number "Set priority (0 is highest): ")))))
    (ir--update-column ir--current-id "priority" priority)
    (message "IR: priority of %s set to %s" ir--current-id priority)))

;;;###autoload
(defun ir-validate ()
  "Report queued ids whose Org heading no longer exists; mutate nothing.
Precondition: a session is active."
  (interactive)
  (ir--require-session)
  (let ((orphans (cl-remove-if #'org-id-find-id-file (ir--all-ids))))
    (message "IR: %d orphan(s)%s" (length orphans)
             (if orphans (format ": %S" orphans) ""))))

;; --- Navigation -------------------------------------------------------------

;;;###autoload
(defun ir-navigate-to-heading (&optional id)
  "Jump to the Org heading for ID, widened then narrowed.
ID defaults to the org-id at point; messages and does nothing when none is
available.  Precondition: a session is active; ID resolves to an existing
heading or file node."
  (interactive)
  (ir--require-session)
  (let ((id (or id (ir--id-at-point))))
    (if id
        (progn (org-id-open id nil) (ir--narrow-to-item))
      (message "IR: no org-id at point"))))

;; --- View & maintenance -----------------------------------------------------

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
(defun ir-edit ()
  "Edit one scheduling column of a queued item chosen by completion.
The id column is immutable; editing `interval' reschedules `due' to now
plus that many days, and editing `due' sets the next date directly.
Precondition: a session is active."
  (interactive)
  (ir--require-session)
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

;; --- Done & delete ----------------------------------------------------------

(defun ir--id-file (id)
  "Return the file containing org-id ID, or nil, without visiting any file.
Uses the Org-roam database when available, else `org-id-locations'."
  (or (and (require 'org-roam nil t)
           (ignore-errors (org-roam-node-file (org-roam-node-from-id id))))
      (org-id-find-id-file id)))

(defun ir--queued-ids-in-file (file)
  "Return the queued ids whose heading lives in FILE."
  (let ((true (file-truename file)))
    (cl-remove-if-not
     (lambda (id)
       (let ((f (ir--id-file id)))
         (and f (equal (file-truename f) true))))
     (ir--all-ids))))

(defun ir--backlink-count (id)
  "Return how many notes link to ID via Org-roam, or 0 when unavailable.
Reads the roam database; visits no file."
  (or (and (require 'org-roam nil t)
           (ignore-errors
             (let ((node (org-roam-node-from-id id)))
               (and node (length (org-roam-backlinks-get node :unique t))))))
      0))

(defun ir--log-done (id title)
  "Append a completion record for ID/TITLE to `ir-done-log-file'.
No-op when `ir-done-log-file' is nil."
  (when ir-done-log-file
    (write-region
     (format "- %s :: %s (%s)\n" (format-time-string "%F %T") (or title "?") id)
     nil (expand-file-name ir-done-log-file) 'append 'silent)))

(defun ir--done-delete-file (id file siblings)
  "Trash FILE, drop ID and SIBLINGS rows, clear the roam db, kill the buffer.
Postcondition: FILE is gone (to trash when `ir-delete-to-trash'); no queue row
references FILE; no live buffer visits it."
  (let ((buf (find-buffer-visiting file)))
    (dolist (sid (cons id siblings)) (ir--delete sid))
    (when (and (require 'org-roam nil t) (fboundp 'org-roam-db-clear-file))
      (ignore-errors (org-roam-db-clear-file file)))
    (delete-file file ir-delete-to-trash)
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))
    (message "IR: deleted %s and %d row(s)"
             (abbreviate-file-name file) (1+ (length siblings)))))

(defun ir--done-cut-subtree (id)
  "Cut the subtree at point to the `kill-ring', save the file, and drop ID's row.
Postcondition: the heading's subtree is removed from the file (recoverable via
`yank' within the session); ID has no queue row."
  (org-back-to-heading t)
  (org-cut-subtree)
  (save-buffer)
  (ir--delete id)
  (message "IR: cut subtree (saved to kill-ring) and removed %s" id))

;;;###autoload
(defun ir-done-and-delete ()
  "Complete the item under review: delete it and its row, then advance.
Acts on `ir--current-id'.  A file-level node deletes its file; a heading cuts
its subtree, keeping the file.  Logs to `ir-done-log-file' and confirms --
disclosing other queued items in the file and incoming backlinks -- before
deleting.  Opens the next due item afterward.
Precondition: a session is active with an item under review."
  (interactive)
  (ir--require-current)
  (let ((id ir--current-id))
    (condition-case nil
        (org-id-open id nil)
      (error (user-error "IR: cannot open %s" id)))
    (let* ((file-level (org-before-first-heading-p))
           (title (if file-level
                      (ir--id-title id)
                    (save-excursion (org-back-to-heading t)
                                    (org-get-heading t t t t))))
           (file (and file-level (or (buffer-file-name) (ir--id-file id))))
           (siblings (and file-level (remove id (ir--queued-ids-in-file file))))
           (backlinks (ir--backlink-count id)))
      (when (and file-level (not file))
        (user-error "IR: cannot resolve the file for %s" id))
      (when (yes-or-no-p
             (format "%s%s%s? "
                     (if file-level
                         (format "Delete file %s%s" (abbreviate-file-name file)
                                 (if ir-delete-to-trash " (to trash)" ""))
                       (format "Cut subtree \"%s\" (to kill-ring)" title))
                     (if siblings
                         (format " -- also drops %d queued row(s) here"
                                 (length siblings))
                       "")
                     (if (> backlinks 0)
                         (format " -- %d backlink(s) will dangle" backlinks)
                       "")))
        (ir--log-done id title)
        (if file-level
            (ir--done-delete-file id file siblings)
          (ir--done-cut-subtree id))
        (ir--open-next)))))

;;;###autoload
(defun ir-open ()
  "Open a queued item chosen by title, narrowed for reading.
Precondition: a session is active."
  (interactive)
  (ir--require-session)
  (let ((id (ir--read-id "Open item: ")))
    (unless (eq (ir--reading-setup (ir--item id)) 'ok)
      (message "IR: cannot open %s (heading missing or id not indexed)" id))))

;;;###autoload
(defun ir-find-item-at-point ()
  "Echo the queue row for the org-id of the heading at point.
Precondition: a session is active."
  (interactive)
  (ir--require-session)
  (let ((id (ir--id-at-point)))
    (message "%S" (and id (ir--item id)))))

;; --- Generative card creation (OpenAI via gptel) ----------------------------

(defcustom ir-gen-card-count 3
  "Number of flashcards `ir-gen-basic'/`ir-gen-cloze' request by default.
A numeric prefix argument to either command overrides this."
  :type 'integer)

(defcustom ir-gen-use-context nil
  "If non-nil, send surrounding text as context to improve generation.
The context source is `ir-gen-context-source'."
  :type 'boolean)

(defcustom ir-gen-context-source 'buffer
  "Where `ir-gen-use-context' draws context from.
Only `buffer' (the whole current buffer) is supported."
  :type '(choice (const buffer)))

(defcustom ir-gen-context-property "IR_CONTEXT"
  "Org property whose value prefixes each generated card as its context.
Read with inheritance, so a file-level #+PROPERTY or an ancestor drawer
applies.  Set to nil to emit cards without a context prefix."
  :type '(choice string (const nil)))

(defconst ir--gen-prompts-directory
  (expand-file-name
   "prompts/"
   (file-name-directory (or load-file-name (locate-library "ir") default-directory)))
  "Directory holding the card-generation prompt templates.
Resolved at load time relative to this file; the recipe must ship `prompts/'.")

(defun ir--gen-load-prompt (filename)
  "Return the contents of FILENAME under `ir--gen-prompts-directory'.
Precondition: FILENAME exists there -- a packaging invariant, not user input.
Signals `file-missing' otherwise (a build/recipe bug)."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename ir--gen-prompts-directory))
    (buffer-string)))

(defun ir--gen-context-text ()
  "Return buffer text to send as LLM context, or nil when context is off.
Honors `ir-gen-use-context' and `ir-gen-context-source'."
  (when ir-gen-use-context
    (pcase ir-gen-context-source
      ('buffer (buffer-substring-no-properties (point-min) (point-max)))
      (other (user-error "IR: unknown `ir-gen-context-source': %S" other)))))

(defun ir--gen-prefix ()
  "Return the card-context prefix from `ir-gen-context-property', or nil.
Reads the property with inheritance; nil when the var or the property is unset."
  (when ir-gen-context-property
    (org-entry-get nil ir-gen-context-property t)))

(defun ir--gen-build-system (kind has-context card-count)
  "Build the system prompt for KIND (`basic' or `cloze').
HAS-CONTEXT non-nil appends the matching context template.  CARD-COUNT is
folded into an exact-count instruction.
Concatenates system.md + the kind file (+ the context file) + instructions."
  (let ((system (ir--gen-load-prompt "system.md"))
        (kind-file (ir--gen-load-prompt (pcase kind
                                          ('basic "basic.md")
                                          ('cloze "cloze.md"))))
        (context (when has-context
                   (ir--gen-load-prompt (pcase kind
                                          ('basic "context.md")
                                          ('cloze "context-cloze.md"))))))
    (concat system "\n\n" kind-file
            (when context (concat "\n\n" context))
            "\n\n# Instructions\n\n"
            (format "Generate EXACTLY %d flashcards from the provided text. " card-count)
            (format "Return EXACTLY %d items in the JSON array — no more, no fewer. " card-count)
            "Do not exceed this count even if the content seems to warrant more cards. "
            "Return only the JSON array, with no prose and no code fences.")))

(defun ir--gen-parse (response)
  "Parse model RESPONSE into a list of card plists.
Strips Markdown code fences, then reads a JSON array of objects with keyword
keys (:q/:a for basic, :c for cloze).
Signals `user-error' when RESPONSE is not parseable JSON (a model fault)."
  (let ((cleaned (string-trim
                  (replace-regexp-in-string "```[a-zA-Z]*" ""
                                            (string-trim response)))))
    (condition-case nil
        (json-parse-string cleaned :object-type 'plist :array-type 'list)
      (error (user-error "IR: could not parse model response as JSON")))))

(defun ir--gen-normalize-question (q)
  "Ensure Q ends with a single question mark (ASCII `?' or Arabic `؟').
Appends `?' only when Q ends with neither."
  (let ((q (string-trim q)))
    (if (string-match-p "[?؟]\\'" q) q (concat q "?"))))

(defun ir--gen-format (kind cards prefix)
  "Format CARDS (plists) of KIND into ankifier-ready plain text.
PREFIX, when a non-empty string, is prepended as \"PREFIX: \" so ankifier's
context-question parsing keeps it as the card context.
Basic: \"[PREFIX: ]Question? Answer\".  Cloze: \"[PREFIX: ]<cloze text>\".
Cards are separated by a blank line."
  (let ((pre (if (and prefix (not (string-empty-p prefix)))
                 (concat prefix ": ")
               "")))
    (mapconcat
     (lambda (card)
       (pcase kind
         ('basic (concat pre
                         (ir--gen-normalize-question (plist-get card :q))
                         " " (string-trim (plist-get card :a))))
         ('cloze (concat pre (string-trim (plist-get card :c))))))
     cards "\n\n")))

(defun ir--gen-insert (text end)
  "Insert TEXT after buffer position END, separated by a blank line.
Postcondition: text before END is unchanged; point sits at TEXT's start; the
mark is deactivated."
  (goto-char end)
  (unless (bolp) (insert "\n"))
  (insert "\n")
  (let ((start (point)))
    (insert text "\n")
    (goto-char start))
  (deactivate-mark))

(defun ir--gen (kind card-count)
  "Generate CARD-COUNT KIND (`basic'/`cloze') flashcards from the active region.
Asynchronous via `gptel-request'; the callback inserts ankifier-ready text
after the region.  KIND content goes to the model with the built system prompt;
when `ir-gen-use-context' is on, a JSON {content, context} wrapper is sent.
Precondition: an active region.  Declines via `user-error' otherwise."
  (unless (use-region-p)
    (user-error "IR: no active region to generate from"))
  (let* ((content (buffer-substring-no-properties (region-beginning) (region-end)))
         (end (region-end))
         (context (ir--gen-context-text))
         (prefix (ir--gen-prefix))
         (system (ir--gen-build-system kind (and context t) card-count))
         (user-text (if context
                        (json-serialize `(:content ,content :context ,context))
                      content))
         (buffer (current-buffer)))
    (message "IR: generating %d %s card(s)…" card-count kind)
    (gptel-request user-text
      :system system
      :callback
      (lambda (response info)
        (if (stringp response)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (ir--gen-insert (ir--gen-format kind (ir--gen-parse response) prefix)
                                end)
                (message "IR: inserted %s card(s)" kind)))
          (message "IR: generation failed: %s"
                   (or (plist-get info :status) "no response")))))))

;;;###autoload
(defun ir-gen-basic (&optional count)
  "Generate basic Q&A flashcards from the active region via OpenAI.
With a numeric prefix COUNT, request that many; otherwise `ir-gen-card-count'.
Inserts ankifier-ready \"[Context: ]Question? Answer\" text after the region."
  (interactive "P")
  (ir--gen 'basic (if (integerp count) count ir-gen-card-count)))

;;;###autoload
(defun ir-gen-cloze (&optional count)
  "Generate cloze flashcards from the active region via OpenAI.
With a numeric prefix COUNT, request that many; otherwise `ir-gen-card-count'.
Inserts ankifier-ready cloze text (with {{cN::…}}) after the region."
  (interactive "P")
  (ir--gen 'cloze (if (integerp count) count ir-gen-card-count)))

(provide 'ir)
;;; ir.el ends here
