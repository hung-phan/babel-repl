# Babeljs repl for emacs

## Dependencies

- `babel`: `npm install -g babel`

## Installation

### Melpa

Coming soon

### Manual install

- Clone it into your `.emacs.d` and add it into load path

```lisp
(add-to-list 'load-path "~/.emacs.d/babeljs-repl.el/")
```

- Load the library with `require`

```lisp
(require 'babeljs-repl)
```

### Weird characters

```lisp
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))
```

## Variables

Change these variables based on your demand

- `babel-cli-program`: the name of babel node cli program, default to
`babel-node`. Change it to absolute path if it's not in your load path.

```lisp
(setq babel-cli-program "/path/to/babel-node")
```

- `babel-cli-arguments`: the list of arguments to pass to `babel-node`.

- `babel-pop-to-buffer`: whether to pop up the babel shell buffer after sending
command to execute.

- `babel-pop-to-buffer-function`: the function used for pop up the babel node
buffer if the above variable is set to t. Default is `pop-to-buffer`. An example
is `pop-to-buffer` to pop up the babel buffer to current window
instead of other window.

## Commands

- `babel-repl`: start a babel node process
- `babel-send-current-region`: send the active region to babel node process
- `babel-send-buffer`: send the whole buffer to babel node process
- `babel-send-paragraph`: send the paragraph at point to babel node process
- `babel-send-region-or-buffer`: send the current region if active, otherwise send
the whole buffer to babel node process
- `babel-send-dwim`: send the current region if active, otherwise send the
paragraph at point to babel node process
- `babel-switch-to-buffer`: switch to babel node buffer if exist, otherwise,
start a new one

## Connect to a babel shell with option

```lisp
(setq babel-cli-arguments '("--trace-sync-io"))
```

## Demo

![Babeljs shell inside Emacs](interaction.gif)
