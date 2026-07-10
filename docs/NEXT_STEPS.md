# ChugakuEigo - Next Steps

## Open items

- Fix README.md: "bcrypt-style hashing" is inaccurate — admin password uses SHA-256 + salt (`constants.R` `ADMIN`)

## Deferred

- Resume issue #14 (per-user learning parameter persistence): branch `feature/user-specific-config-persistence` still lacks the settings-save UI. Master and the live `users_meta` sheet are both on the 8-column schema now, so resumption is safe. (Deferred since 2026-06-08 after the schema-drift incident; re-listed 2026-07-11.)
