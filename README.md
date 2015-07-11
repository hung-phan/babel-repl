# Babel REPL for emacs

[![MELPA](http://melpa.org/packages/babel-repl-badge.svg)](http://melpa.org/#/babel-repl)

## Acknowledgement

Thanks tmtxt for his plugin [n4js.el](https://github.com/tmtxt/n4js.el)

## Dependencies

- `babel`: `npm install -g babel`

## Installation

### Melpa

[babel-repl](http://melpa.org/#/babel-repl)

### Manual install

- Clone it into your `.emacs.d` and add it into load path

```lisp
(add-to-list 'load-path "~/.emacs.d/babel-repl/")
```

- Load the library with `require`

```lisp
(require 'babel-repl)
```

### Weird characters workaround

If you get weird characters in your REPL. Add this code snippet to your configuaration file.

```lisp
(require 'comint)
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))
```

## Variables

Change these variables based on your demand

- `babel-repl-cli-program`: the name of babel node cli program, default to
`babel-node`. Change it to absolute path if it's not in your load path.

```lisp
(setq babel-repl-cli-program "/path/to/babel-node")
```

- `babel-repl-cli-arguments`: the list of arguments to pass to `babel-node`.

- `babel-repl-pop-to-buffer`: whether to pop up the babel shell buffer after sending
command to execute.

- `babel-repl-pop-to-buffer-function`: the function used for pop up the babel node
buffer if the above variable is set to t. Default is `pop-to-buffer`. An example
is `pop-to-buffer` to pop up the babel buffer to current window
instead of other window.

## Commands

- `babel-repl`: start a babel node process
- `babel-repl-send-current-region`: send the active region to babel node process
- `babel-repl-send-buffer`: send the whole buffer to babel node process
- `babel-repl-send-paragraph`: send the paragraph at point to babel node process
- `babel-repl-send-region-or-buffer`: send the current region if active, otherwise send
the whole buffer to babel node process
- `babel-repl-send-dwim`: send the current region if active, otherwise send the
paragraph at point to babel node process
- `babel-repl-switch-to-buffer`: switch to babel node buffer if exist, otherwise,
start a new one

## Connect to a babel shell with option

```lisp
(setq babel-repl-cli-arguments '("--trace-sync-io"))
```

## Demo

![Babel shell inside Emacs](interaction.gif)
