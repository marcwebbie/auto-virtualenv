;;; auto-virtualenv.el --- Automatically activate Python virtualenvs based on project directory -*- lexical-binding: t; -*-

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; Maintainer: Marcwebbie <marcwebbie@gmail.com>
;; URL: https://github.com/marcwebbie/auto-virtualenv
;; Version: 2.2.0
;; Keywords: python, virtualenv, environment, tools, projects
;; Package-Requires: ((cl-lib "0.5"))
;; License: GPL-3.0-or-later

;;; Commentary:
;;
;; Auto Virtualenv is a powerful Emacs package for Python developers, offering
;; automatic virtual environment management based on the directory of the current
;; project. This tool simplifies working across multiple Python projects by
;; dynamically detecting and activating virtual environments, reducing the need
;; for manual configuration.
;;
;; It integrates seamlessly with `lsp-mode` and `pyright`, optionally reloading
;; the LSP workspace upon environment activation to maintain accurate imports and
;; environment settings. Auto Virtualenv identifies Python projects using a
;; customizable set of markers (e.g., `setup.py`, `pyproject.toml`) and supports
;; common virtual environment locations, both local and global (e.g., `~/.pyenv/versions/`).
;;
;; Features:
;; - **Automatic Virtual Environment Detection and Activation**: Based on project root,
;;   auto-virtualenv locates and activates virtual environments in either a local
;;   project directory or in specified global directories.
;; - **LSP Reload Support**: With `lsp-mode` or `pyright`, optionally reload the LSP workspace
;;   on environment changes to keep code assistance up-to-date.
;; - **Modeline Integration**: Displays the active environment in the modeline. When no
;;   environment is active, "Venv: N/A" is shown.
;; - **Configurable and Extensible**: Users can add directories for environment searches, set
;;   custom project markers, and control verbosity for debugging.
;;
;; Usage:
;; 1. Add `auto-virtualenv` to your `load-path` and enable it with `auto-virtualenv-setup`.
;; 2. Configure `auto-virtualenv-global-dirs`, `auto-virtualenv-python-project-files`,
;;    and `auto-virtualenv-reload-lsp` as needed.
;; 3. Use it with project management packages like `projectile` or independently.
;;
;; See the README for detailed setup and configuration examples.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'projectile)

(defgroup auto-virtualenv nil
  "Automatically activate Python virtual environments."
  :group 'python)

(defcustom auto-virtualenv-global-dirs
  '("~/.virtualenvs/" "~/.pyenv/versions/" "~/.envs/" "~/.conda/" "~/.conda/envs/")
  "List of global directories to search for virtual environments by project name."
  :type '(repeat string)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-python-project-files
  '("requirements.txt" "Pipfile" "pyproject.toml" "setup.py" "manage.py" "tox.ini" ".flake8" "pytest.ini"
    ".pre-commit-config.yaml" "environment.yml" "__init__.py")
  "List of files that identify a Python project."
  :type '(repeat string)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-activation-hooks
  '(find-file-hook projectile-after-switch-project-hook)
  "Hooks that trigger virtual environment activation."
  :type '(repeat symbol)
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-reload-lsp t
  "Automatically reload `lsp-mode` or `pyright` when changing virtual environments."
  :type 'boolean
  :group 'auto-virtualenv)

(defcustom auto-virtualenv-verbose t
  "Enable verbose output for debugging."
  :type 'boolean
  :group 'auto-virtualenv)

(defvar auto-virtualenv-current-virtualenv nil
  "The currently activated virtual environment.")
(defvar auto-virtualenv-last-project nil
  "The last project directory that was processed to prevent redundant checks.")
(defvar auto-virtualenv-original-mode-line mode-line-format
  "The original mode line format to revert to when no virtualenv is active.")
(defvar auto-virtualenv-mode-line "Venv: N/A"
  "String to display in the mode line for the active virtual environment.")

(defun auto-virtualenv--debug (msg &rest args)
  "Print MSG formatted with ARGS if `auto-virtualenv-verbose' is enabled."
  (when auto-virtualenv-verbose
    (message (apply 'format (concat "[auto-virtualenv] " msg) args))))

(defun auto-virtualenv-update-mode-line ()
  "Update the mode line to show the active virtual environment, or 'N/A' if none."
  (setq auto-virtualenv-mode-line
        (if auto-virtualenv-current-virtualenv
            (propertize (format "[Venv: %s]" (file-name-nondirectory (directory-file-name auto-virtualenv-current-virtualenv)))
                        'face '(:weight bold :foreground "DeepSkyBlue"))
          (propertize "[Venv: N/A]" 'face '(:weight bold :foreground "DimGray"))))
  (setq global-mode-string (list auto-virtualenv-mode-line))
  (force-mode-line-update t))

(defun auto-virtualenv-read-python-version (project-root)
  "Read the virtual environment name from .python-version file in PROJECT-ROOT, if present."
  (let ((version-file (expand-file-name ".python-version" project-root)))
    (when (file-readable-p version-file)
      (auto-virtualenv--debug "Virtualenv selected from .python-version file at %s" version-file)
      (string-trim (with-temp-buffer
                     (insert-file-contents version-file)
                     (buffer-string))))))

(defun auto-virtualenv-find-local-venv (project-root)
  "Check for a local virtual environment in PROJECT-ROOT. Return the path if found, otherwise nil."
  (auto-virtualenv--debug "Checking for local virtualenv in %s" project-root)
  (let ((local-venv-path (or (expand-file-name ".venv" project-root)
                             (expand-file-name "venv" project-root))))
    (when (file-directory-p local-venv-path)
      (auto-virtualenv--debug "Found local virtualenv at %s" local-venv-path)
      local-venv-path)))

(defun auto-virtualenv-find-global-venv (env-name)
  "Search for ENV-NAME in `auto-virtualenv-global-dirs`, only at top level of each directory."
  (auto-virtualenv--debug "Searching for %s in global directories" env-name)
  (cl-some (lambda (dir)
             (let ((venv-path (expand-file-name env-name dir)))
               (when (file-directory-p venv-path)
                 (auto-virtualenv--debug "Found global virtualenv in %s" venv-path)
                 venv-path)))
           auto-virtualenv-global-dirs))

(defun auto-virtualenv-is-python-project (project-root)
  "Check if PROJECT-ROOT contains Python project files."
  (auto-virtualenv--debug "Checking if %s has Python project files" project-root)
  (cl-some (lambda (file)
             (file-exists-p (expand-file-name file project-root)))
           auto-virtualenv-python-project-files))

(defun auto-virtualenv-activate (venv-path)
  "Activate the virtual environment at VENV-PATH."
  (auto-virtualenv--debug "Activating virtual environment: %s" venv-path)
  (setq auto-virtualenv-current-virtualenv (file-name-as-directory venv-path))
  (let ((venv-bin (concat auto-virtualenv-current-virtualenv "bin")))
    (setq exec-path (cons venv-bin exec-path))
    (setenv "VIRTUAL_ENV" auto-virtualenv-current-virtualenv)
    (setenv "PATH" (concat venv-bin path-separator (getenv "PATH"))))
  (auto-virtualenv-update-mode-line)
  ;; Reload `lsp-mode` or `pyright` if enabled
  (when (and auto-virtualenv-reload-lsp (bound-and-true-p lsp-mode))
    (auto-virtualenv--debug "Reloading lsp-mode for virtual environment at %s" venv-path)
    (lsp-restart-workspace)))

(defun auto-virtualenv-deactivate ()
  "Deactivate any active virtual environment."
  (when auto-virtualenv-current-virtualenv
    (let ((venv-bin (concat auto-virtualenv-current-virtualenv "bin")))
      ;; Remove the virtualenv bin directory from exec-path and PATH
      (setq exec-path (delete venv-bin exec-path))
      (setenv "PATH" (mapconcat 'identity (delete venv-bin (split-string (getenv "PATH") path-separator)) path-separator))
      (setenv "VIRTUAL_ENV" nil)
      (setq auto-virtualenv-current-virtualenv nil)
      (auto-virtualenv--debug "Virtualenv deactivated"))
    (auto-virtualenv-update-mode-line)))

(defun auto-virtualenv-locate-project-root ()
  "Find the project root using `projectile-project-root` if available, else search for `.git` markers."
  (if (and (featurep 'projectile) (fboundp 'projectile-project-root))
      (projectile-project-root)
    (let ((dir (locate-dominating-file default-directory
                                       (lambda (parent)
                                         (cl-some (lambda (marker)
                                                    (file-exists-p (expand-file-name marker parent)))
                                                  '(".git" "setup.py" "Pipfile" "pyproject.toml"))))))
      (if dir
          (expand-file-name dir)
        (auto-virtualenv--debug "No project root found.")
        nil))))

(defun auto-virtualenv-find-and-activate ()
  "Find and activate a virtual environment based on the current project."
  (let* ((project-root (auto-virtualenv-locate-project-root)))
    (if (or (not project-root)
            (equal project-root auto-virtualenv-last-project))
        (progn
          (auto-virtualenv--debug "Skipping activation as project root has not changed or is empty.")
          ;; Always update the mode line, even if activation is skipped
          (auto-virtualenv-update-mode-line))
      (setq auto-virtualenv-last-project project-root)
      (if (auto-virtualenv-is-python-project project-root)
          (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
                 (env-name (or (auto-virtualenv-read-python-version project-root) project-name))
                 (venv-path (or (auto-virtualenv-find-local-venv project-root)
                                (auto-virtualenv-find-global-venv env-name))))
            (if venv-path
                (auto-virtualenv-activate venv-path)
              (auto-virtualenv-deactivate)))
        (auto-virtualenv-deactivate)))))

(defun auto-virtualenv-setup ()
  "Setup auto-virtualenv with user-defined hooks."
  (dolist (hook auto-virtualenv-activation-hooks)
    (add-hook hook #'auto-virtualenv-find-and-activate)))

(provide 'auto-virtualenv)

;;; auto-virtualenv.el ends here
