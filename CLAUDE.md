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
# cl-lib); org-roam is require'd lazily inside ir-add-roam-node, so -Q suffices.
emacs -Q --batch -L . -f batch-byte-compile ir.el && rm -f ir.elc

# Docstring/style conventions:
emacs -Q --batch --eval '(progn (require (quote checkdoc)) (checkdoc-file "ir.el"))'
```

The datastore, algorithm, priority-queue, column whitelist, and migration are
**testable headlessly** — bind `ir-db-location` to a temp file, set `ir--db` to
nil, and call the `ir--*` functions in `emacs -Q --batch`. Extraction needs a
**file-visiting** buffer (`org-id-get-create` errors in `with-temp-buffer`), so
test it via `find-file` on a temp `.org` file. Interactive session/frame/UI flows
(`ir-start-session`, `ir-view`, completing-read prompts) still need manual checks.

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
priority % wins). `ir-read-next`/`ir-end-session` reschedule the *current* item
then advance.

### Extraction: promote in place (copy)

`ir-extract-region` copies the active region into a new **child heading** of the
enclosing heading (`ir--extract-make-child`), gives it an `org-id`, and enqueues
it. The parent text is retained; recursion falls out (extract from an extract).

### Reading & views

`ir--reading-setup` is the single open path: `org-id-open` + widen + narrow (no
type dispatch). Missing-heading orphans are reported and optionally deleted.
`ir-view` renders a due-ordered Org table; `ir-edit`/`ir-delete`/`ir-open`
complete over `"title — id"` labels resolved by `ir--id-title`.

## Conventions & gotchas

- **Naming:** `ir-*` = interactive commands; `ir--*` = private helpers; `defcustom ir-*` = settings.
- **Dynamic-SQL safety:** SQLite parameterizes *values* only, not identifiers. `ir--update-column` whitelists the column against `ir--editable-columns` before interpolation — keep that guard for any caller-chosen column.
- **org-roam is a soft dependency:** core runs on `org-id` (covers roam *and* plain Org files); org-roam is `require`d lazily, only in `ir-add-roam-node`.
- **Datastore is user data, not repo data:** `~/org/ir.db` lives outside the repo and is configurable via `defcustom ir-db-location`.
- **Extraction needs a file-visiting buffer** (org-id constraint) — relevant when writing tests.
