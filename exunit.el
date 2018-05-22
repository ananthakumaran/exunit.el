;;; exunit.el --- ExUnit test runner -*- lexical-binding: t -*-

;; Copyright (C) 2015 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; URL: http://github.com/ananthakumaran/exunit.el
;; Version: 0.1
;; Keywords: elixir exunit
;; Package-Requires: ((dash "2.10.0") (s "1.11.0"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'ansi-color)
(require 'compile)

;;; Private

(defcustom exunit-mix-test-default-options '()
  "List of options that gets passed to the mix test command."
  :type '(repeat string)
  :group 'exunit)

(defcustom exunit-environment '()
  "List of environment variables used when running mix test.
Each element should be a string of the form ENVVARNAME=VALUE."
  :type '(repeat (string :tag "ENVVARNAME=VALUE"))
  :group 'exunit)

(defmacro exunit-def-permanent-buffer-local (name &optional init-value)
  "Declare NAME as buffer local variable."
  `(progn
     (defvar ,name ,init-value)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(exunit-def-permanent-buffer-local exunit-project-root nil)

(defun exunit-project-root ()
  (or
   exunit-project-root
   (let ((root (locate-dominating-file default-directory "mix.exs")))
     (unless root
       (error "Couldn't locate project root folder. Make sure the current file is inside a the project."))
     (setq exunit-project-root (expand-file-name root))
     exunit-project-root)))

(defun exunit-project-name ()
  (file-name-nondirectory (directory-file-name (exunit-project-root))))

(defun exunit-dependency-filename (dep filename)
  (let ((project-name (exunit-project-name)))
    (cond
     ((member dep '("elixir" "stdlib")) filename)
     ((string= dep project-name) filename)
     (t (s-join "/" (list "deps" dep filename))))))

(defun exunit-parse-error-filename (filename)
  (let ((match (s-match "(\\([^)]*\\)) \\(.*\\)" filename)))
    (if match
        (exunit-dependency-filename (nth 1 match) (nth 2 match))
      filename)))

(defun exunit-test-filename ()
  (file-relative-name (buffer-file-name) (exunit-project-root)))

(defun exunit-test-filename-line-number ()
  (concat (exunit-test-filename) ":" (number-to-string (line-number-at-pos))))

(defun exunit-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(define-compilation-mode exunit-compilation-mode "ExUnit Compilation"
  "Compilation mode for ExUnit output."
  (setq compilation-parse-errors-filename-function #'exunit-parse-error-filename)
  (add-hook 'compilation-filter-hook 'exunit-colorize-compilation-buffer nil t))


(defun exunit-compile (args)
  (let ((default-directory (exunit-project-root))
        (compilation-environment exunit-environment))
    (compile
     (s-join " " (-concat '("mix" "test") exunit-mix-test-default-options args))
     'exunit-compilation-mode)))

;;; Public

(defun exunit-verify-all ()
  (interactive)
  (exunit-compile '()))

(defun exunit-verify-single ()
  (interactive)
  (exunit-compile (list (exunit-test-filename-line-number))))

(defun exunit-verify ()
  (interactive)
  (exunit-compile (list (exunit-test-filename))))

(define-minor-mode exunit-mode
  "Minor mode for ExUnit test runner"
  :lighter " exunit")

(provide 'exunit)

;;; exunit.el ends here
