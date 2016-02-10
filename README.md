# auto-virtualenv

Automatically activate python virtualenvs on emacs based on project name or `.python-version` file.

## Installation

### manual

Clone this repository somewhere and add this directory to you
`load-path`.

## Configuration

```
(require 'auto-virtualenv')
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
```

## Alternatives

+ [pyenv-mode-auto](https://github.com/ssbb/pyenv-mode-auto)
