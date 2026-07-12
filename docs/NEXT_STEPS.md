# ChugakuEigo - Next Steps

## Open items

- Fix README.md: "bcrypt-style hashing" is inaccurate — admin password uses SHA-256 + salt (`constants.R` `ADMIN`)
- Review auto-generated grammar tags: `data/qlist.csv` `grammar` column was populated by the rule-based classifier in `scripts/tag_grammar.py` (a reviewable draft). Known coarse spots: be動詞主動詞 sometimes falls back to `basic_svo`; present-perfect in questions with intervening words is under-detected; `-ed` adjectives partially stop-listed. Spot-check and correct via the classifier or by editing the column, then re-check the "weak grammar points" dashboard.
- Consider persisting per-user `grammar_alpha` alongside the other `param_*` columns when issue #14 (user-specific config persistence) resumes — the config knob exists (`config_state$grammar_alpha`) but is session-only for now.

## Deferred

- Resume issue #14 (per-user learning parameter persistence): branch `feature/user-specific-config-persistence` still lacks the settings-save UI. Master and the live `users_meta` sheet are both on the 8-column schema now, so resumption is safe. (Deferred since 2026-06-08 after the schema-drift incident; re-listed 2026-07-11.)
