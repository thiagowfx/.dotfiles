# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Build/Lint/Test
- Install dotfiles: `just stow`
- Remove dotfiles: `just unstow`
- Check for dangling symlinks: `just stow-lint`
- Update git submodules and pre-commit hooks: `just update`
- Run pre-commit hooks: `pre-commit run --all-files`
- Run specific pre-commit hook: `pre-commit run --all-files <hook-id>`
- Run ansible playbook: `just ansible`

## Code Style Guidelines

### General
- Follow shell best practices; code is linted with shellcheck
- Maintain alphabetical ordering for lists with `keep-sorted` markers
- Use ShellCheck for shell script linting (excluding .zsh files)
- Use 2 space indentation for YAML files
- No trailing whitespace
- Properly format JSON files

### Git Practices
- Use descriptive commit messages
- Ensure submodules are updated with `just update`
- All GitHub Actions workflows should pin dependencies to specific commit hashes