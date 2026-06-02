# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`ir.el` is an Emacs Lisp package implementing Incremental Reading (the SuperMemo
technique by Piotr Wozniak) over **Org material only** — plain Org headings and
Org-roam nodes, keyed by `org-id`. It schedules items with a SuperMemo-style
topic algorithm and extracts fragments in place. The repo is a single `.el` file
plus docs; there is no build system or package manager.

PDF/web/video/bibtex support was removed in v0.13.0 (PDF incremental reading
moved to a separate app, `~/code/electric-card-maker`).

## Files

- `ir.el` — the entire package.
- `plans/` — design/spec notes (e.g. `v0.13.0-refactor.org`), tracked as org WBS checklists.

## Development

Emacs 30.2 is on PATH. There is no CI; verify with byte-compile, checkdoc, and headless batch tests.

```sh
# Byte-compile — must be warning-free. Built-in deps only (org, org-id, sqlite,
# cl-lib); org-roam is require'd lazily (roam commands + `ir--reading-setup`), so -Q suffices.
emacs -Q --batch -L . -f batch-byte-compile ir.el && rm -f ir.elc

# Docstring/style conventions:
emacs -Q --batch --eval '(progn (require (quote checkdoc)) (checkdoc-file "ir.el"))'
```

The datastore, algorithm, priority-queue, column whitelist, and migration are
**testable headlessly** — bind `ir-db-location` to a temp file, set `ir--db` to
nil, and call the `ir--*` functions in `emacs -Q --batch`. Extraction needs a
**file-visiting** buffer (`org-id-get-create` errors in `with-temp-buffer`), so
test it via `find-file` on a temp `.org` file. Interactive session/frame/UI flows
(`ir-start-session`, `ir-end-session`, completing-read prompts) still need manual checks.

Git default branch is `master`; active work happens on `development`.

## Architecture

### Data model: schedule (SQLite) vs. content (Org), linked by org-id

The SQLite database (native `sqlite.el`, default `~/org/ir.db`) stores **only the
schedule** — one row per item in table `ir`. The content is the Org heading the
`id` points at. Columns:

| Column          | Type    | Notes                                            |
|-----------------|---------|--------------------------------------------------|
| `id`            | TEXT PK | `org-id` of the heading/node                     |
| `afactor`       | REAL    | A-Factor, default 1.2                            |
| `interval`      | INTEGER | last interval in days; informational + edit knob |
| `priority`      | REAL    | 0 = most important … 100 = least; default 50     |
| `due`           | INTEGER | next-due Unix seconds                            |
| `last_reviewed` | INTEGER | last-review Unix seconds                         |

Rows decode to **plists** via `ir--row->item` (`:id :afactor :interval :priority
:due :last_reviewed`); `ir--columns` is the canonical SELECT column list whose
order must match the decoder. There is no positional `(nth N)` access and no
`type`/`path` column — every item is a uniform org-id pointer.

### Connection is lazy

`ir--db` opens, memoizes, creates-or-migrates on **first use** — never at load
time. A pre-0.13 database (has `type`, lacks `due`) is rebuilt by
`ir--migrate-from-old` (backs up to `<file>.bak`, reconstructs `last_reviewed`).

### Scheduling: SuperMemo topic algorithm

`ir--reschedule` uses the **real elapsed interval**: `interval = max(1,
round((now − last_reviewed)/86400 × afactor))`, `afactor += ir-afactor-increment`,
`due = now + interval days`. Selection (`ir--query-due`) is a **priority queue**:
`WHERE due <= now ORDER BY priority ASC, due ASC LIMIT 1` (due-ness gates, lower
priority % wins). The **item under review is `ir--current-id`** — session state
set by `ir--open-next`, not point — so `ir-read-next`/`ir-set-priority`/`ir-end-session`/`ir-done-and-delete`
act on the opened item regardless of point drift or which buffer is current.

### Extraction: promote in place (copy)

`ir-extract-region` copies the active region into a new **child heading** of the
enclosing heading (`ir--extract-make-child`), gives it an `org-id`, and enqueues
it. The parent text is retained; recursion falls out (extract from an extract).

### Reading & views

`ir--reading-setup` opens via `org-id-open`, then `ir--narrow-to-item`:
`org-narrow-to-subtree` for a heading, or the **whole file** for a file-level node
(`org-before-first-heading-p`). Only an `org-id-open` resolution failure returns
`unresolved`; presentation errors (e.g. narrowing) never do. On `unresolved`,
`ir--open-next` offers to prune the stale row — it never auto-deletes.
`ir-edit`/`ir-open` complete over `"title — id"` labels from `ir--id-title`, which
reads the **org-roam db** (or the id) and never visits files.

### Completion: `ir-done-and-delete`

Acts on `ir--current-id`, navigates to it, logs to
`ir-done-log-file`, then deletes: a file-level node's file → trash
(`ir-delete-to-trash`) plus its sibling queue rows and roam-db entry; a heading →
`org-cut-subtree`. It advances via `ir--open-next` afterward. The confirm
discloses sibling-row and backlink counts; foreign backlinks are left dangling
(not repaired).

## Conventions & gotchas

- **Naming:** `ir-*` = interactive commands; `ir--*` = private helpers; `defcustom ir-*` = settings.
- **Dynamic-SQL safety:** SQLite parameterizes *values* only, not identifiers. `ir--update-column` whitelists the column against `ir--editable-columns` before interpolation — keep that guard for any caller-chosen column.
- **Session gating:** every interactive command except `ir-start-session` and the importers (`ir-add`, `ir-add-roam-node`, `ir-add-roam-directory`) requires an active session — `ir--require-session` (active) or `ir--require-current` (active + an item under review) signal a `user-error` otherwise. `ir--session-active`/`ir--session-count`/`ir--session-start-time` hold the session state; `ir-start-session` sets them, `ir-end-session` resets them and reports items-read / elapsed / still-due.
- **Review-item model:** `ir--current-id` (not point) is the item under review; `ir-read-next`/`ir-set-priority`/`ir-done-and-delete` act on it. Point-reading commands (`ir-add`, `ir-navigate-to-heading`, `ir-find-item-at-point`) still resolve their target via `ir--id-at-point`.
- **`org-id-get` only in Org buffers:** Org 9.7+ routes it through `org-element-at-point`, which errors elsewhere; `ir--id-at-point` guards with `derived-mode-p`.
- **Autoloads:** every interactive command carries `;;;###autoload`; `doom sync` regenerates `ir-autoloads.el` so `M-x` sees them without a config loader.
- **org-roam is a soft dependency:** core runs on `org-id`; org-roam is `require`d lazily (roam importers, `ir--reading-setup`, `ir--id-title`) and its `org-id-find` advice resolves roam ids from the roam db.
- **Datastore is user data, not repo data:** `~/org/ir.db` lives outside the repo and is configurable via `defcustom ir-db-location`.
- **Extraction needs a file-visiting buffer** (org-id constraint) — relevant when writing tests.
