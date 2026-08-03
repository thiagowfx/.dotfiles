#!/usr/bin/env python3
"""Web search adapted from pasky/pi-amplike's MIT-licensed web-search skill."""

import sys
import urllib.parse
import urllib.request
from urllib.error import HTTPError, URLError

TIMEOUT = 30


def main() -> None:
    if len(sys.argv) < 2:
        print("Usage: search.py <query>", file=sys.stderr)
        raise SystemExit(1)

    query = " ".join(sys.argv[1:]).strip()
    if not query:
        print("Error: query cannot be empty", file=sys.stderr)
        raise SystemExit(1)

    encoded = urllib.parse.quote_plus(query)
    url = f"https://r.jina.ai/http://html.duckduckgo.com/html/?q={encoded}"
    headers = {
        "Accept": "text/plain",
        "User-Agent": "pi-web-search/1.0",
    }

    request = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(request, timeout=TIMEOUT) as response:
            content = response.read().decode("utf-8", errors="replace").strip()
    except HTTPError as error:
        print(f"Error: HTTP {error.code} - {error.reason}", file=sys.stderr)
        raise SystemExit(1) from error
    except URLError as error:
        print(f"Error: {error.reason}", file=sys.stderr)
        raise SystemExit(1) from error

    if not content:
        print("No search results found. Try a different query.")
        return

    print("## Search Results\n")
    print(content)


if __name__ == "__main__":
    main()
