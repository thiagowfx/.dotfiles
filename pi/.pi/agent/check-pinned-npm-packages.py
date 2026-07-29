#!/usr/bin/env python3
"""Reject Pi npm package specs that do not use an exact version."""

import json
import re
import sys
from pathlib import Path

SETTINGS = Path("pi/.pi/agent/settings.json")
PINNED_NPM_SPEC = re.compile(
    r"^npm:(?:@[^/@\s]+/[^@/\s]+|[^@/\s]+)@"
    r"v?(?:0|[1-9]\d*)\.(?:0|[1-9]\d*)\.(?:0|[1-9]\d*)"
    r"(?:-[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?"
    r"(?:\+[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?$"
)


def npm_specs(packages: list[object]) -> list[str]:
    specs = []
    for package in packages:
        if isinstance(package, str) and package.startswith("npm:"):
            specs.append(package)
        elif isinstance(package, dict):
            source = package.get("source")
            if isinstance(source, str) and source.startswith("npm:"):
                specs.append(source)
    return specs


def main() -> int:
    settings = Path(sys.argv[1]) if len(sys.argv) == 2 else SETTINGS
    try:
        packages = json.loads(settings.read_text())["packages"]
    except (OSError, json.JSONDecodeError, KeyError) as error:
        print(f"Cannot read Pi packages from {settings}: {error}", file=sys.stderr)
        return 1

    if not isinstance(packages, list):
        print(f"Pi packages must be a list in {settings}", file=sys.stderr)
        return 1

    unpinned = [spec for spec in npm_specs(packages) if not PINNED_NPM_SPEC.fullmatch(spec)]
    if not unpinned:
        return 0

    print("Pi npm packages must use exact versions:", file=sys.stderr)
    print(*[f"  {spec}" for spec in unpinned], sep="\n", file=sys.stderr)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
