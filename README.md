# gomacro-mode

[![CI](https://github.com/storvik/gomacro-mode/workflows/CI/badge.svg)](https://github.com/storvik/gomacro-mode/actions)

This Emacs package provides bindings for working with [Gomacro](https://github.com/cosmos72/gomacro), a read eval print loop for Go.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [gomacro-mode](#gomacro-mode)
- [Todo](#todo)
- [Issues](#issues)
- [Installation](#installation)
- [Usage](#usage)
    - [Interactive functions](#interactive-functions)
        - [`gomacro-run`](#gomacro-run)
        - [`gomacro-verbose-toggle`](#gomacro-verbose-toggle)
        - [`gomacro-eval (stmt)`](#gomacro-eval-stmt)
        - [`gomacro-eval-region (begin end)`](#gomacro-eval-region-begin-end)
        - [`gomacro-eval-line`](#gomacro-eval-line)
        - [`gomacro-eval-defun`](#gomacro-eval-defun)
        - [`gomacro-eval-buffer`](#gomacro-eval-buffer)
    - [Other useful functions](#other-useful-functions)
        - [`gomacro-running-p`](#gomacro-running-p)

<!-- markdown-toc end -->

# Todo

- [ ] Add `gomacro-eval-file` which prompts for file to eval
- [ ] Add `gomacro-eval-pacakge` which evaluates all files in specified folder
- [ ] Look into using `go--build-font-lock-keywords`to to syntax highlightning in gomacro buffer
- [x] Combine eval region and eval region verbose to one function with config param
- [x] Add `gomacro-eval-buffer`
- [x] Add `gomacro-eval-line`
- [x] Add support for type definitions in eval defun (struct and interface)
- [x] Remove all comments in `gomacro--sanitize-string`
- [x] Ensure only one whitespace in `gomacro--sanitize-string`
- [x] Add some tests
- [x] Handle package declarations

# Issues

Does not handle multiple packages as all files are evaluated without package header.

# Installation

This is a work in progress and currently not in MELPA.
Installation can be done using [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package gomacro-mode
    :straight (:host github :repo "storvik/gomacro-mode")
    :hook (go-mode . gomacro-mode))
```

# Usage

By enabling `gomacro-mode` in buffer with Go source code the following keybindings will be set:

| Keybinding | Function               |
|------------|------------------------|
| C-M-x      | gomacro-eval-defun     |
| C-c C-r    | gomacro-eval-region    |
| C-c C-l    | gomacro-eval-line      |
| C-c C-t    | gomacro-verbose-toggle |

## Interactive functions

The following functions are ment to be used from the `M-x` interface.

### `gomacro-run`

Run an inferior instance of `gomacro` inside Emacs `comint-mode` buffer

### `gomacro-verbose-toggle`

Toggle value of `gomacro-verbose-toggle`.
When verbose mode is activated everything is sent to gomacro line by line, default behaviour is to only display "Region is sent to gomacro REPL".

### `gomacro-eval (stmt)`

Evaluate `stmt` in gomacro REPL buffer.
Will prompt for statement if run interactively.

### `gomacro-eval-region (begin end)`

Evaluate region between `begin` and `end`.
Can be run interactively with marked region.
If `gomacro-verbose-eval` is set region will be sent line by line to gomacro REPL.

### `gomacro-eval-line`

Evaluate current line.

### `gomacro-eval-defun`

Evaluate the nearest function, type or import statement looking backwards.
This function will select whichever function, type or import statement that is nearest to current cursor position and pass it to gomacro REPL.

### `gomacro-eval-buffer`

Evaluate entire buffer.

> Does not evaluate package statement, hence all buffers sent to gomacro REPL will be in the same namespace.

### `gomacro-eval-file`

Evaluate file, will prompt for file if run interactively.

### `gomacro-eval-package`

Evaluate go package, will prompt for path if run interactively.
If filename is specified all other go files in same directory will be processed.

## Other useful functions

These functions are not ment to run interactively.

### `gomacro-running-p`

Check if gomacro REPL is running or not.
