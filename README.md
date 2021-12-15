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

```elisp
(require 'auto-virtualenv)
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
```

Optionally:

```elisp
;; Activate on changing buffers
(add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
;; Activate on focus in
(add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)

```

## How it works

On `auto-virtualenv-set-virtualenv`, the hierarchy is scanned for a
special file, which is part of the list
`auto-virtualenv-project-root-files`. The list contains files which
indicate a project root, like a `.git` directory or a `.dir-locals.el`
file.

In order, files and paths to check for virtualenv to activate:

- 0. `.auto-virtualenv-version` eg: `~/.virtualenvs/hello_world`
- 1. `.python-version` eg: `3.8.0`
- 2. Try `.venv` dir in the root of project
- 3. Try finding a virtualenv with the same name of Project Root in `~/.virtualenvs/`.

If a `auto-virtualenv-version` file is found then activate it from its path in the contents.
If a `.python-version` file exists, the contents of the file joined to
the project root, form the location of the virtualenv. Otherwise if a
.venv directory is found in the project root, this directory is
used. Otherwise a directory within `~/.virtualenvs` or
`~/.pyenv/versions/`, having with the project's name, which is the
directory name of the project root, is checked for being a virtual
env.

The found environment is finally activated using `pyvenv-activate`.

## Alternatives

+ [pyenv-mode-auto](https://github.com/ssbb/pyenv-mode-auto)

## License

[![GNU GPL v3.0](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl.html)

View official GNU site <http://www.gnu.org/licenses/gpl.html>.
