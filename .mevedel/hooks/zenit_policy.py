#!/usr/bin/env python3
"""Small mevedel hook policies for the Zenit Emacs workspace."""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from typing import Any, Iterable


PROTECTED_EXACT = {
    "init.el",
    "init.elc",
    ".mevedel/input-history.el",
}

PROTECTED_PREFIXES = (
    ".local/",
    "auto-save-list/",
    "straight/build",
    "straight/repos/",
    ".mevedel/sessions/",
)

PACKAGE_PATH_RE = re.compile(
    r"(^|/)(packages\.el)$|^straight/versions/.*\.el$"
)

COMMIT_RE = re.compile(r"(^|[;&|()\s])git\s+commit(\s|$)")


def _read_event() -> dict[str, Any]:
    try:
        raw = sys.stdin.read()
        return json.loads(raw) if raw.strip() else {}
    except json.JSONDecodeError:
        return {}


def _tool_input(event: dict[str, Any]) -> dict[str, Any]:
    value = event.get("tool_input") or event.get("toolInput") or {}
    return value if isinstance(value, dict) else {}


def _candidate_paths(tool_input: dict[str, Any]) -> Iterable[str]:
    for key in ("file_path", "filePath", "path", "directory"):
        value = tool_input.get(key)
        if isinstance(value, str):
            yield value


def _workspace_relative(path: str) -> str:
    root = Path.cwd().resolve()
    candidate = Path(path).expanduser()
    if not candidate.is_absolute():
        candidate = root / candidate
    try:
        rel = candidate.resolve().relative_to(root)
    except ValueError:
        return candidate.as_posix()
    return rel.as_posix()


def _emit(decision: dict[str, Any]) -> None:
    if decision:
        print(json.dumps(decision, separators=(",", ":")))


def generated_file_guard(event: dict[str, Any]) -> None:
    for path in _candidate_paths(_tool_input(event)):
        rel = _workspace_relative(path)
        if rel in PROTECTED_EXACT or any(rel.startswith(prefix) for prefix in PROTECTED_PREFIXES):
            _emit(
                {
                    "permissionDecision": "deny",
                    "permissionReason": (
                        f"{rel} is generated or runtime-local state in this Zenit workspace. "
                        "Edit source files, tracked site-lisp files, or approved .mevedel skills/hooks instead."
                    ),
                }
            )
            return


def package_change_context(event: dict[str, Any]) -> None:
    touched = [_workspace_relative(path) for path in _candidate_paths(_tool_input(event))]
    if any(PACKAGE_PATH_RE.search(path) for path in touched):
        _emit(
            {
                "systemMessage": (
                    "Package or lockfile change detected. After package declarations or recipes change, "
                    "run `bin/emacs-config refresh`; for intentional revision updates use "
                    "`bin/emacs-config sync -u` and `bin/emacs-config freeze` when lockfiles should change."
                )
            }
        )


def commit_message_context(event: dict[str, Any]) -> None:
    command = _tool_input(event).get("command")
    if isinstance(command, str) and COMMIT_RE.search(command):
        _emit(
            {
                "systemMessage": (
                    "Before committing, follow `docs/commit-messages.md` and `.gitmessage`: "
                    "`type(component): Capitalized active-verb summary`, "
                    "plus GNU-style file bullets for full messages."
                )
            }
        )


def main(argv: list[str]) -> int:
    actions = {
        "generated-file-guard": generated_file_guard,
        "package-change-context": package_change_context,
        "commit-message-context": commit_message_context,
    }
    if len(argv) != 2 or argv[1] not in actions:
        print(f"usage: {argv[0]} <{'|'.join(actions)}>", file=sys.stderr)
        return 64
    actions[argv[1]](_read_event())
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
