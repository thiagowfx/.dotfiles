## 2024-05-20 - Inefficient fzf plugin loading in zsh

**Learning:** The `fzf` plugin was being loaded by checking for the existence of `fzf` completion and key-binding files in multiple common locations. This resulted in several unnecessary `stat` calls on every shell startup, slowing it down. The official `fzf` documentation recommends a single, more efficient command for shell integration.

**Action:** When configuring shell plugins, always consult the official documentation for the recommended setup. Avoid redundant file system checks in startup scripts by using the suggested integration methods, which are typically optimized for performance.
