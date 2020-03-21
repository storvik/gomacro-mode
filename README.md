# gomacro-mode

[![CI](https://github.com/storvik/gomacro-mode/workflows/CI/badge.svg)](https://github.com/storvik/gomacro-mode/actions)

**WORK IN PROGRESS**

This Emacs package provides bindings for working with [Gomacro](https://github.com/cosmos72/gomacro), a read eval print loop for Go.
It aims to provide some of the most used features in Slime etc.


## TODO

- [x] Combine eval region and eval region verbose to one function with config param
- [x] Add `gomacro-eval-buffer`
- [x] Add `gomacro-eval-line`
- [x] Add support for type definitions in eval defun (struct and interface)
- [x] Remove all comments in `gomacro--sanitize-string`
- [x] Ensure only one whitespace in `gomacro--sanitize-string`
- [ ] Handle package declarations
- [ ] Add some tests
