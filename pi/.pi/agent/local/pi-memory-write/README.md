# pi-memory-write

Minimal curated-memory package for Pi.

- Registers only `memory_write`.
- Appends explicit durable facts and preferences to `~/.pi/agent/memory/MEMORY.md`.
- Injects at most 4,000 characters from `MEMORY.md` into each session.
- Does not generate session summaries, daily logs, scratchpads, embeddings, or background jobs.
- Avoids duplicate exact entries.

Set `PI_MEMORY_FILE` to override the memory path, primarily for isolated tests.
