# ChugakuEigo - Lessons Learned

Project-specific lessons. Record failures, constraints, and pitfalls discovered during development.

> **Distribution criteria**: If a lesson is a general pattern applicable across projects, promote it to `knowledge/` using `/kaizen-reflect-learning`. Keep only project-specific lessons here.

## Entry Format

```markdown
### [Short title] (YYYY-MM-DD)

- **Situation**: What you were trying to do
- **Problem**: What went wrong or what constraint was discovered
- **Lesson**: What to do differently next time
- **Promoted**: → knowledge/path.md § Section  <!-- optional -->
```

If a lesson has been promoted to `knowledge/`, add a trailing `- **Promoted**:` bullet so future cross-project audits (`/kaizen-audit-knowledge`) skip it as already covered.

## Lessons

### users_meta schema drift broke production initialization (2026-06-08)

- **Situation**: shinyapps.io app initialization failed with "Length of col_types is not compatible".
- **Problem**: The unmerged branch `feature/user-specific-config-persistence` (issue #14) had already applied its schema change to the **live** `users_meta` sheet (added 4 `param_*` columns → 8 columns), while deployed master still read with `col_types = "cDDd"` (4 columns). The live sheet's schema ran ahead of the deployed code.
- **Lesson**: `users_meta` is 8 columns (`username, created_date, last_updated, question_count, param_range_min, param_range_max, param_prob_base, param_zero_limit`, col_types `"cDDdiidi"`). Keep the live sheet's column count and the code's col_types in lockstep, and never apply live-sheet schema changes from unmerged branches. Master was fixed to the 8-column schema on 2026-06-08; resuming #14 is safe (the branch still lacks the settings-save UI — see NEXT_STEPS.md).

### Deploy "succeeded" but app failed to start — shiny missing from manifest (2026-06-08)

- **Situation**: `rsconnect::deployApp()` completed, but the app crashed on shinyapps.io with "The shiny package was not found in the library" (exit status 1).
- **Problem**: rsconnect builds the deployment manifest from **locally installed** packages. Local R lacked `shiny`, so shiny/httpuv were silently absent from the manifest.
- **Lesson**: Install all app dependencies locally and run the app once before deploying (now codified as the pre-deploy checklist in CLAUDE.md § Deployment Security). Machine-specific gotcha: `/usr/bin/Rscript` (R 4.5) carried stale pre-4.0 packages (e.g. `mime`) that failed to load — reinstall from Posit PPM Linux binaries with `options(HTTPUserAgent = ...)` and `repos = "https://packagemanager.posit.co/cran/__linux__/bookworm/latest"`. Verify startup afterwards with `rsconnect::showLogs()`.
