" ALE: Asynchronous Lint Engine:
"   Syntax checking and semantic errors
"   Language Server Protocol (LSP) client
"   :ALEFix to apply linter fixes
"   :ALEInfo to debug/troubleshoot
let g:ale_fixers = {
      \    'python': ['autopep8'],
      \}
let g:ale_fix_on_save = 1
