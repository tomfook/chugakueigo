---
project:
  id: "chugakueigo"
  name: "ChugakuEigo"
  status: released          # planning | developing | released | completed | on-hold
  keywords:
    - R
    - shiny
    - shinyapps.io
    - googlesheets4
    - adaptive-learning
    - language-learning
  created: "2026-02-21"
  updated: "2026-07-11"
---

# ChugakuEigo

## Purpose

Japanese-to-English translation practice app ("Middle School English"). Helps Japanese speakers practice translating Japanese sentences into English, with an adaptive learning system that surfaces the user's weakest questions more often.

## Overview

R Shiny web application. A user selects their name, starts a session, and is shown Japanese sentences to translate mentally; after revealing the answer they self-assess with OK/NG. Question selection is probabilistic — inverse exponential weighting on each user's per-question score, with configurable weakness bias, question range, and a cap on zero-scored questions. Questions/answers are keyword-shuffled to prevent rote memorization.

In production on shinyapps.io as `chugakueigo-v2` (original v0.1 launched 2018-08-28). Scores persist to Google Sheets via a service account.

## Tech Stack

- **Language/Framework**: R, Shiny (server.R / ui.R + 5 independent modules)
- **Packages**: dplyr, DT, googlesheets4
- **Persistence**: `data/qlist.csv` (question DB) + Google Sheets (per-user score worksheets, `users_meta` sheet)
- **Auth**: Google Cloud service account (`credentials/service-account-key.json`)
- **Hosting**: shinyapps.io (deploy via rsconnect, exclusions via exact-name `.rscignore`)

## Design Decisions

- **Google Sheets for score persistence** (not local CSV): shinyapps.io's filesystem is ephemeral, so scores must live off-instance. `data/score.csv` was retired.
- **Module independence**: modules never source or call each other; `server.R` is the sole orchestration layer; only `constants.R` (config) and `utils.R` (infrastructure) are shared. Namespaced function prefixes (`data_*`, `learning_*`, …) map functions to files.
- **question_id-based scoring**: scores are keyed to stable question IDs (not row positions), so questions can be inserted/deleted safely; missing IDs are zero-padded at read time.
- **Grammar-unit adaptation reuses per-question scores** (no new stored score): each question carries `grammar` tags (`data/qlist.csv`, `|`-separated; taxonomy in `data/grammar_list.csv`). A grammar's weakness `ḡ_g` = mean of its member questions' effective scores; selection uses a blended exponent `s̃_i = α·s_i + (1−α)·min_g ḡ_g` (config `grammar_alpha`, UI slider). `α=1` (default) is byte-for-byte identical to the legacy per-question formula, so the feature ships dark and is opted into per session. Grammar logic lives in `learning_engine.R` (`learning_build_grammar_meta`, `learning_grammar_scores`, `learning_blended_weakness`); tags are precomputed offline by `scripts/tag_grammar.py` (data-prep only, deploy-excluded).
- **Logical deletion** (`is_active` flag) for questions and users instead of destructive removal.
- **Global cache** (`app_global_cache`, TTL 300s) for `sheet_names`/`users_meta` reads to keep Sheets API usage within quota across sessions.
- **Structured error returns** (`list(success, data, message)`) everywhere instead of `stop()`, so the UI can always show a user-facing notification.

## Notes

- `users_meta` sheet is **8 columns** (`cDDdiidi`); live-sheet schema and code col_types must stay in lockstep — drift broke production once (see [LEARNINGS.md](./LEARNINGS.md)).
- Deploy preflight is mandatory: rsconnect builds the manifest from locally installed packages (see CLAUDE.md § Deployment Security).
- Admin password (user-deletion gate) and its change procedure are in `ADMIN_INFO.md` (git-untracked, deploy-excluded).

## Project Documentation

- `PROJECT_SUMMARY.md` - Project overview (this file)
- [CODING_STANDARDS.md](./CODING_STANDARDS.md) - Full coding standards
- [LEARNINGS.md](./LEARNINGS.md) - Project-specific lessons learned
- [NEXT_STEPS.md](./NEXT_STEPS.md) - Open and deferred work items
