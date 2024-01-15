# auto-virtualenv

Automatically activate python virtualenvs on emacs based on project name or `.python-version` file.

## Installation


### MELPA

`auto-virtualenv` is available on [MELPA](https://melpa.org).

You can install `auto-virtualenv` with the following command.

<kbd>M-x package-install [RET] auto-virtualenv [RET]</kbd>

### Manual

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

With `use-package`

```elisp
(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)  ;; If using projectile
  )
```

## How it works

On `auto-virtualenv-set-virtualenv`, the hierarchy is scanned for a
special file, which is part of the list
`auto-virtualenv-project-root-files`. The list contains files which
indicate a project root, like a `.git` directory or a `.dir-locals.el`
file.

In order, files and paths to check for virtualenv to activate:

- 0. Try path set from `auto-virtualenv-custom-virtualenv-path` variable
- 1. Try path from `.auto-virtualenv-version` file if it exists or
- 2. Try name from `.python-version` file if it exists or
- 3. Try `.venv` or `.virtualenv` or `venv` dir in the root of project
- 4. Try finding a virtualenv with the same name as Project in virtualenv dirs set in `auto-virtualenv-dir`.
- 4. Try common virtualenv paths in home dir: `~/.virtulenvs` and `~/.pyenv/versions`

The found environment is finally activated using `pyvenv-activate`.

## License

[![GNU GPL v3.0](http://www.gnu.org/graphics/gplv3-127x51.png)](http://www.gnu.org/licenses/gpl.html)

View official GNU site <http://www.gnu.org/licenses/gpl.html>.
