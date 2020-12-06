;;; cells.el --- Utilities for code split into cells -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Augusto Stoffel

;; Version: 0.0
;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Maintainer: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/cells.el
;; Keywords: convenience, cells
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you efficiently navigate, edit and execute code
;; split into cells according to certain magic comments.  Such files
;; can be obtained, for instance, by exporting a Jupyter notebook to a
;; script.
;;
;; The simplest entry point of the package is the `cells-command'
;; function.  It takes as argument a command that can act on a region,
;; and returns an anonymous command that acts on the current cell.
;; Thus, one can redefine C-c C-c in Python mode to evaluate the
;; current cell by doing the following:
;;
;;     (define-key
;;       python-mode-map
;;       (kbd "C-c C-c")
;;       (cells-command 'python-shell-send-region))
;;
;; See the README for more examples, including a ready-to-use setup
;; for Jupyter and a handy hydra.

;;; Code:

(require 'pulse)
(require 'rx)

(defgroup cells nil
  "Utilities for code split into cells."
  :group 'convenience
  :prefix "cells-")

(defcustom cells-cell-markers
  '("%%"
    (regexp "In\\s-*\\[.*?\\]"))
  "A list of regular expressions in sexp form (see `rx').
Each of regexp should match the content of a comment line which
introduces a cell break."
  :type '(repeat sexp))

(defface cells-header-line '((t :extend t :inherit header-line))
  "Face used by `cells-mode' to highlight cell boundaries.")

(defun cells-boundary-regexp ()
  "Return a regexp matching comment lines that serve as cell boundary."
  (rx line-start
      (+ (syntax comment-start))
      (* (syntax whitespace))
      (or (eval (cons 'or cells-cell-markers)))))

(defun cells-forward-cell (&optional arg)
  "Move to the next cell boundary, or end of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
backward."
  (interactive "p")
  (let ((page-delimiter (cells-boundary-regexp)))
    (forward-page arg)
    (move-beginning-of-line 1)))

(defun cells-backward-cell (&optional arg)
  "Move to the previous cell boundary, or beginning of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (cells-forward-cell (- (or arg 1))))

(defmacro cells-do (&rest body)
  "Find current cell bounds and evaluate BODY.
Inside BODY, the variables `beg' and `end' are bound to the
limits of the current cell.

If the first element of BODY is the keyword `:use-region' and the
region is active, use its bounds instead."
  `(pcase (if (and ,(eq (car body) :use-region) (use-region-p))
              (list t (region-end) (region-beginning))
            (save-excursion
              (list nil
                    (progn (cells-forward-cell) (point))
                    (progn (cells-backward-cell) (point)))))
     (`(,using-region ,end ,beg)
      ,@body)))

(defun cells-mark-cell ()
  "Put point at the beginning of this cell, mark at end."
  ;; TODO: add arg; extend region when active
  (interactive)
  (cells-do
   (goto-char beg)
   (push-mark end nil t)))

(defun cells-command (fun &optional docstring &rest options)
  "Returns an anonymous command that calls FUN on the current cell.

FUN is a function that takes two character positions as argument.
Most interactive commands that act on a region are of this form
and can be used here.

If OPTIONS contains the keyword :use-region, the command will act
on the region instead of the current cell if appropriate.

If OPTIONS contains the keyword :pulse, provide visual feedback
via `pulse-momentary-highlight-region'."
  (declare (doc-string 2))
  (unless (stringp docstring)
    (setq options (cons docstring options))
    (setq docstring (concat
                     "Call `" (symbol-name fun) "' on the current code cell."
                     (when (member :use-region options)
                       "\nIf region is active, use it instead."))))
  (eval `(lambda ()
          ,docstring
          (interactive)
          (cells-do ,(car (member :use-region options))
                    ,(when (member :pulse options)
                       '(pulse-momentary-highlight-region beg end))
                    (funcall ',fun beg end)))))

;;;###autoload
(define-minor-mode cells-mode
  "Minor mode for cell-oriented code."
  ;; TODO: integrate with outline-mode?
  :keymap (make-sparse-keymap)
  (let ((spec `((,(concat "\\(" (cells-boundary-regexp) "\\).*\n")
                 0 'cells-header-line append))))
    (if cells-mode
        (progn
          (font-lock-add-keywords nil spec))
      (font-lock-remove-keywords nil spec))
    (font-lock-flush)))

(provide 'cells)
;;; cells.el ends here
