---
name: web-search
description: >-
  Search the web for documentation, facts, current information, or relevant pages
  using keyless Jina Reader and DuckDuckGo.
---

# Web Search

Run:

```bash
{baseDir}/search.py "search query"
```

Use specific queries. Search iteratively only when initial results are weak.
For plain search requests, return search results without fetching pages.
Use `web_fetch` only when user asks to inspect page contents or search snippets are insufficient.
