# Sentinel's Journal

This journal is for CRITICAL security learnings only.

## 2026-01-11 - Proactive Secret Scanning with Talisman
**Vulnerability:** Lack of proactive secret scanning in the repository.
**Learning:** While no secrets were found during a manual audit, the repository lacked a mechanism to prevent them from being committed in the future.
**Prevention:** Implemented Talisman with a high-sensitivity threshold to automatically scan for and block secrets before they are committed.
## YYYY-MM-DD - [Title]
**Vulnerability:** [What you found]
**Learning:** [Why it existed]
**Prevention:** [How to avoid next time]
