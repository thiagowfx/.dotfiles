# Local pi-memory fork

Based on [`pi-memory@0.4.0`](https://pi.dev/packages/pi-memory).

## Exit summaries

On `Ctrl-D` or `/quit`, this fork synchronously saves a JSON job under:

```text
~/.pi/agent/memory/exit-summary-jobs/
```

It does not call an LLM or qmd during Pi shutdown. Next Pi session claims queued jobs and generates summaries in the
background with the active model. Completed summaries are written to the daily log for date of original session; failed
jobs remain queued for retry.

A job claimed by a Pi process that dies is retried after ten minutes.

`README.upstream.md` contains upstream pi-memory documentation.
