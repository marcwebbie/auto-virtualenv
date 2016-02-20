# auto-virtualenv

Automatically activate python virtualenvs on emacs based on project name or `.python-version` file.

## Installation


### MELPA

`auto-virtualenv` is available on [MELPA](https://melpa.org).

You can install `auto-virtualenv` with the following command.

<kbd>M-x package-install [RET] auto-virtualenv [RET]</kbd>

### manual

Clone this repository somewhere and add this directory to you
`load-path`.

## Configuration

``` elisp
(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
```

Optionally:

```
;; Activate on changing buffers
(add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
;; Activate on focus in
(add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)

```

## Alternatives

+ [pyenv-mode-auto](https://github.com/ssbb/pyenv-mode-auto)

## License

[![GNU GPL v3.0](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl.html)

View official GNU site <http://www.gnu.org/licenses/gpl.html>.
