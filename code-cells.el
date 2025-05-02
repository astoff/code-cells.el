;;; * code-cells.el --- Lightweight notebooks with support for ipynb files -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023  Free Software Foundation, Inc.

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: convenience, outlines
;; URL: https://github.com/astoff/code-cells.el
;; Package-Requires: ((emacs "27.1") (compat "29.1"))
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; * Commentary:

;; With this package, you can efficiently navigate, edit and execute
;; code split into cells according to certain magic comments.  It also
;; allows you to open ipynb notebook files directly in Emacs.  They
;; will be automatically converted to a script for editing, and
;; converted back to notebook format when saving.  An external tool,
;; Jupytext by default, is required for this.
;;
;; A minor mode, `code-cells-mode', provides the following features:
;;
;; - Fontification of cell boundaries.
;;
;; - Keybindings for the cell navigation and evaluation commands,
;;   under the `C-c %' prefix.
;;
;; - Outline mode integration: cell headers have outline level
;;   determined by the number of percent signs or asterisks; within a
;;   cell, outline headings are as determined by the major mode, but
;;   they are demoted by an amount corresponding to the level of the
;;   containing cell.  This provides code folding and hierarchical
;;   navigation, among other things, when `outline-minor-mode' is
;;   active.
;;
;; This minor mode is automatically activated when opening an ipynb
;; file, but you can also activate it in any other buffer, either
;; manually or through a hook.

;;; * News:

;; Version 0.5
;; - Several new editing commands.
;; - Integrate with repeat-mode and context-menu-mode.
;; - Some changed keybindings.
;; - More consistent handling of numeric arguments and cell ranges.

;;; * Code:

(require 'outline)
(require 'pulse)
(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist)
  (require 'rx))

(defgroup code-cells nil
  "Utilities for code split into cells."
  :group 'convenience
  :prefix "code-cells-")

(defcustom code-cells-boundary-regexp
  (rx line-start
      (+ (syntax comment-start))
      (or (seq (* (syntax whitespace)) "%" (group-n 1 (+ "%")))
	  (seq (* (syntax whitespace)) "*")
          (seq " In[" (* (any space digit)) "]:")))
  "Regular expression specifying cell boundaries.
It should match at the beginning of a line.  The length of the
first capture determines the outline level."
  :type 'regexp
  :safe #'stringp)

(defface code-cells-header-line '((t :extend t
                                     :overline t
                                     :inherit font-lock-comment-face))
  "Face used by `code-cells-mode' to highlight cell boundaries.")

; highlight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom code-cells-highlight-cell t
  "Non-nil tells Code-Cells mode to highlight the current cell."
  :type 'boolean
  :group 'aesthetics
  :safe 'booleanp)

(defface code-cells-highlight-face
  '((t :inherit org-block
       :weight bold
       :extend t))
  "Default face for highlighting the current cell."
  :group 'aesthetics)

(defvar code-cells-overlay nil
  "Overlay used by Code-Cells mode to highlight the current cell.")
(make-variable-buffer-local 'code-cells-overlay)

(defcustom code-cells-highlight-face 'code-cells-highlight-face
  "Face with which to highlight the current cell."
  :type 'face
  :group 'aesthetics
  :set (lambda (symbol value)
         (set symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when code-cells-overlay
               (overlay-put code-cells-overlay 'face code-cells-highlight-face))))))

(defcustom code-cells-sticky-flag nil
  "Non-nil means the Code-Cells mode highlight appears in all windows.
Otherwise Code-Cells mode will highlight only in the selected
window.  Setting this variable takes effect the next time you use
the command `code-cells-mode' to turn Code-Cells mode on."
  :type 'boolean
  :group 'aesthetics)

; highlight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom code-cells-major-mode-outline-min-level 0
  "Minimal level of major-mode outline headings.
`code-cells-mode' integrates with `outline-minor-mode' by combining
major-mode-defined outline levels with cell boundaries.  Major mode
headings are demoted by at least this amount."
  :type 'natnum
  :safe #'numberp)

(defcustom code-cells-eval-region-commands
  `((drepl--current . drepl-eval-region)
    (jupyter-repl-interaction-mode . ,(apply-partially 'jupyter-eval-region nil))
    (python-mode . python-shell-send-region)
    (python-ts-mode . python-shell-send-region) ;For Emacs ≤ 29
    (emacs-lisp-mode . eval-region)
    (lisp-interaction-mode . eval-region))
  "Alist of commands to evaluate a region.
The keys are major or minor modes and the values are functions
taking region bounds as argument."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom code-cells-convert-ipynb-style
  '(("jupytext" "--to" "ipynb")
    ("jupytext" "--to" "auto:percent")
    code-cells--guess-mode
    code-cells-convert-ipynb-hook)
  "Determines how to convert ipynb files for editing.
The first two entries are lists of strings: the command name and
arguments used, respectively, to convert to and from ipynb format.

The third entry is a function called with no arguments to determine the
major mode to be called after conversion from ipynb format.  The default
setting tries to guess it from the notebook metadata.

The fourth entry, also optional, is a hook run after the new major mode
is activated."
  :type '(list (repeat string) (repeat string) function symbol))

(defcustom code-cells-convert-ipynb-hook '(code-cells-mode)
  "Hook run after converting an ipynb notebook to a regular script."
  :type 'hook)

;;; * Cell navigation

;;;###autoload
(defun code-cells-forward-cell (&optional arg)
  "Move to the next cell boundary, or end of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
backward."
  (interactive "p")
  (unless arg (setq arg 1))
  (unless (zerop arg)
    (when (or (cl-plusp arg) (not (bolp)))
      (goto-char (pos-eol)))
    (condition-case nil
        (dotimes (_ (abs arg))
          (re-search-forward code-cells-boundary-regexp
                             nil nil (cl-signum arg)))
      (search-failed (goto-char (if (cl-plusp arg) (point-max) (point-min))))
      (:success (when (cl-plusp arg) (goto-char (match-beginning 0)))))))

(put 'code-cell 'forward-op #'code-cells-forward-cell) ;For thing at point

;;;###autoload
(defun code-cells-backward-cell (&optional arg)
  "Move to the previous cell boundary, or beginning of buffer.
With ARG, repeat this that many times.  If ARG is negative, move
forward."
  (interactive "p")
  (code-cells-forward-cell (- (or arg 1))))

(defun code-cells--bounds (&optional count use-region no-header)
  "Return the bounds of the current code cell, as a 2-element list.

If COUNT is non-nil, return instead a region containing COUNT cells and
starting or ending with the current cell, depending on the sign of
COUNT.  The values 0 and -1 have a special meaning: they stand for the
half of the current cell below respectively above the current line.

If USE-REGION is non-nil and the region is active, return the
region bounds instead.

If NO-HEADER is non-nil, do not include the cell boundary line."
  (if (and use-region (use-region-p))
      (list (region-beginning) (region-end))
    (setq count (or count 1))
    (save-excursion
      (let ((bol (pos-bol))
            (end (progn (code-cells-forward-cell
                         (if (cl-plusp count) count 1))
                        (point))))
        (code-cells-backward-cell (abs count))
        (and no-header
             (looking-at-p code-cells-boundary-regexp)
             (forward-line))
        (pcase count
          (0 (list bol end))
          (-1 (list (point) bol))
          (_ (list (point) end)))))))

(defun code-cells--neighbor-bounds (distance)
  "Return the bounds of the cell DISTANCE cells away from the current one."
  (save-excursion
    (unless (looking-at-p code-cells-boundary-regexp)
      (code-cells-backward-cell))
    (code-cells-forward-cell distance)
    (code-cells--bounds)))

;;; * Cell Highlighting

(defun code-cells-highlight ()
  "Activate the Code-Cells overlay on the current line."
  (if code-cells-mode  ; Might be changed outside the mode function.
      (let ((current-cell (code-cells--bounds)))
        (progn
          (unless code-cells-overlay
            (setq code-cells-overlay (make-overlay 1 1)) ; to be moved
            (overlay-put code-cells-overlay 'face code-cells-highlight-face))
          (overlay-put code-cells-overlay
                       'window (unless code-cells-sticky-flag (selected-window)))
          ;; Only move overlay when cell boundaries change
          (code-cells-move-ol code-cells-overlay current-cell)))
    (code-cells-unhighlight)))

(defun code-cells-unhighlight ()
  "Deactivate the Code-Cells overlay on the current line."
  (when code-cells-overlay
    (delete-overlay code-cells-overlay)))

(defun code-cells-move-ol (overlay bounds)
  "Move the Code-Cells overlay.
BOUNDS can be provided as (start . end) to avoid recalculation."
  (if bounds
      (move-overlay overlay (car bounds) (cadr bounds))
    (move-overlay overlay 1 1)))

(defun code-cells-setup-cellhighlight ()
  ;; In case `kill-all-local-variables' is called.
  (add-hook 'change-major-mode-hook #'code-cells-unhighlight nil t)
  ;; Remove pre-command hook to prevent flickering
  (remove-hook 'pre-command-hook #'code-cells-unhighlight t)
  ;; Set up post-command hook for highlighting
  (code-cells-highlight)
  (add-hook 'post-command-hook 
            (lambda ()
              (when (sit-for 0.05 t)
                (code-cells-highlight)))
            nil t))

;;; * Command-generating functions

;;;###autoload
(cl-defun code-cells-command (fun &key use-region pulse no-header)
  "Return an anonymous command calling FUN on the current cell.

FUN must be a function that takes two character positions as argument.
Most interactive commands that act on a region are of this form and
can be used here.

If USE-REGION is non-nil, the command will act on the region instead of
the current cell when the region is active.

If PULSE is non-nil, provide visual feedback via
`pulse-momentary-highlight-region'.

If NO-HEADER is non-nil, exclude the cell header from the acted region."
  (lambda (arg)
    (interactive "p")
    (pcase-let ((`(,start ,end) (code-cells--bounds arg use-region no-header)))
      (when pulse (pulse-momentary-highlight-region start end))
      (funcall fun start end))))

;;;###autoload
(defun code-cells-speed-key (command)
  "Return a speed key definition, suitable for passing to `define-key'.
The resulting keybinding will only have any effect when the point
is at the beginning of a cell heading, in which case it executes
COMMAND."
  `(menu-item nil ,command
              :filter ,(lambda (d)
                         (and (bolp)
                              (looking-at code-cells-boundary-regexp)
                              d))))

;;; * Text manipulation commands

;;;###autoload
(defun code-cells-move-cell-down (arg)
  "Move current code cell vertically ARG cells.
Move up when ARG is negative and move down otherwise."
  (interactive "p")
  (pcase-let ((`(,current-beg ,current-end) (code-cells--bounds))
              (`(,next-beg ,next-end) (code-cells--neighbor-bounds arg)))
    (unless (save-excursion
              (and (/= current-beg next-beg)
                   (goto-char current-beg)
                   (looking-at-p code-cells-boundary-regexp)
                   (goto-char next-beg)
                   (looking-at-p code-cells-boundary-regexp)))
      (user-error "Can't move cell"))
    (transpose-regions current-beg current-end next-beg next-end)))

;;;###autoload
(defun code-cells-move-cell-up (&optional arg)
  "Move current code cell vertically up ARG cells."
  (interactive "p")
  (code-cells-move-cell-down (- arg)))

;;;###autoload
(defun code-cells-mark-cell (&optional arg)
  "Put point at the beginning of this cell, mark at end.
If ARG is non-nil, mark that many cells."
  (interactive "p")
  (pcase-let ((`(,start ,end) (code-cells--bounds arg)))
    (goto-char start)
    (push-mark end nil t)))

;;;###autoload(autoload 'code-cells-comment-or-uncomment "code-cells" nil t)
(defalias 'code-cells-comment-or-uncomment
  (code-cells-command #'comment-or-uncomment-region :no-header t)
  "Comment or uncomment the current code cell.
If ARG is provided, act on that many cells.")

;;;###autoload(autoload 'code-cells-indent "code-cells" nil t)
(defalias 'code-cells-indent (code-cells-command #'indent-region :no-header t)
  "Reindent the current code cell.
With a prefix argument, act on that many cells.")

;;;###autoload(autoload 'code-cells-delete "code-cells" nil t)
(defalias 'code-cells-delete (code-cells-command #'delete-region)
  "Delete the current code cell without modifying the kill ring.
With a prefix argument, act on that many cells.")

;;;###autoload(autoload 'code-cells-kill "code-cells" nil t)
(defalias 'code-cells-kill (code-cells-command #'kill-region)
  "Kill (\"cut\") the current code cell, saving it in the kill ring.
With a prefix argument, act on that many cells.")

;;;###autoload(autoload 'code-cells-copy "code-cells" nil t)
(defalias 'code-cells-copy (code-cells-command #'kill-ring-save :pulse t)
  "Save the current code cell to the kill ring.
With a prefix argument, act on that many cells.")

;;;###autoload
(defun code-cells-duplicate (&optional arg)
  "Duplicate the current code cell.
With a prefix argument, act on that many cells."
  (interactive "p")
  (pcase-let* ((`(,start ,end) (code-cells--bounds arg))
               (text (buffer-substring start end)))
    (save-excursion
      (goto-char (if (and arg (cl-minusp arg)) start end))
      (insert text))))

;;; * Code evaluation

;;;###autoload
(defun code-cells-eval (start end)
  "Evaluate code according to current modes.
The first suitable function from `code-cells-eval-region-commands'
is used to do the job.

Interactively, evaluate the region, if active, otherwise the
current code cell.  With a numeric prefix, evaluate that many
code cells.

Called from Lisp, evaluate region between START and END."
  (interactive (code-cells--bounds (prefix-numeric-value current-prefix-arg) t t))
  (funcall
   (or (seq-some (pcase-lambda (`(,mode . ,fun))
                   (when (or (and (boundp mode) (symbol-value mode))
                             (derived-mode-p mode))
                     fun))
                 code-cells-eval-region-commands)
       (user-error
        "No entry for the current modes in `code-cells-eval-region-commands'"))
   start end)
  (pulse-momentary-highlight-region start end))

;;;###autoload
(defun code-cells-eval-and-step (arg)
  "Evaluate the current cell and move to the next one.
With a prefix argument ARG, act on that many cells."
  (interactive "p")
  (pcase-let* ((`(,start ,end) (code-cells--bounds arg nil t)))
    (code-cells-eval start end)
    (if (cl-plusp arg)
        (goto-char end)
      (goto-char start)
      (code-cells-forward-cell -2))))

(defun code-cells--above-or-below-bound (raw)
  "Evaluation bound for the given RAW prefix argument."
  (list
   (pcase (- (prefix-numeric-value raw))
     (0 (pos-bol))
     ((and (pred cl-plusp) arg) (car (code-cells--neighbor-bounds arg)))
     (arg (cadr (code-cells--neighbor-bounds arg))))))

;;;###autoload
(defun code-cells-eval-above (point)
  "Evaluate all cells above the current one.
With a prefix argument, exclude that many extra cells.

From Lisp, just evaluate from beginning of buffer to POINT."
  (interactive (code-cells--above-or-below-bound current-prefix-arg))
  (code-cells-eval (point-min) point))

;;;###autoload
(defun code-cells-eval-below (point)
  "Evaluate the current cell and all below.
With a prefix argument, include that many extra cells.

From Lisp, just evaluate from POINT to end of buffer."
  (interactive (code-cells--above-or-below-bound current-prefix-arg))
  (code-cells-eval point (point-max)))

;;;###autoload
(defun code-cells-eval-whole-buffer ()
  "Evaluate the entire buffer."
  (interactive)
  (code-cells-eval (point-min) (point-max)))

;;; * Minor mode

(defvar-local code-cells--saved-vars nil
  "A place to save variables before activating `code-cells-mode'.")

(defun code-cells--outline-level ()
  "Compute the outline level, taking code cells into account.
To be used as the value of the variable `outline-level'.

At a cell boundary, returns the cell outline level, as determined by
`code-cells-boundary-regexp'.  Otherwise, returns the sum of the
outline level as determined by the major mode and the current cell
level."
  (let* ((at-boundary (looking-at code-cells-boundary-regexp))
         (cell-level (if (or at-boundary
                             (save-excursion
                               (re-search-backward
                                code-cells-boundary-regexp nil t)))
                         (if (match-beginning 1)
                             (- (match-end 1) (match-beginning 1))
                           1)
                       0))
         (mm-level (and (not at-boundary)
                        (looking-at (car code-cells--saved-vars))
                        (funcall (cadr code-cells--saved-vars)))))
    (if mm-level
        (+ (max cell-level code-cells-major-mode-outline-min-level) mm-level)
      cell-level)))

(defun code-cells--font-lock-keywords ()
  "Font lock keywords to highlight cell boundaries."
  `((,(rx (regexp code-cells-boundary-regexp) (* any) "\n")
     0 'code-cells-header-line append)))

(defvar-keymap code-cells--prefix-map
  :repeat t
  "p" #'code-cells-backward-cell
  "n" #'code-cells-forward-cell
  "P" #'code-cells-move-cell-up
  "N" #'code-cells-move-cell-down
  ";" #'code-cells-comment-or-uncomment
  "w" #'code-cells-copy
  "d" #'code-cells-duplicate
  "e" #'code-cells-eval
  "s" #'code-cells-eval-and-step
  "a" #'code-cells-eval-above
  "\\" #'code-cells-indent
  "C-w" #'code-cells-kill
  "@" #'code-cells-mark-cell)

;;;###autoload
(define-minor-mode code-cells-mode
  "Minor mode for cell-oriented code."
  :keymap (let ((map (make-sparse-keymap)))
            (keymap-set map "C-c %" code-cells--prefix-map)
            map)
  (let ((vars '(outline-regexp
                outline-level
                outline-heading-end-regexp
                paragraph-start)))
    (cond
     (code-cells-mode
      (setq-local
       code-cells--saved-vars (mapcar #'symbol-value vars)
       outline-level 'code-cells--outline-level
       outline-regexp (rx (or (regexp code-cells-boundary-regexp)
                              (regexp outline-regexp)))
       outline-heading-end-regexp "\n"
       paragraph-separate (rx (or (regexp paragraph-separate)
                                  (regexp code-cells-boundary-regexp))))
      (add-hook 'context-menu-functions 'code-cells--context-menu 20 t)
      (font-lock-add-keywords nil (code-cells--font-lock-keywords))
      (when code-cells-highlight-cell (code-cells-setup-cellhighlight)))
     (t
      (dolist (var vars)
        (set (make-local-variable var) (pop code-cells--saved-vars)))
      (remove-hook 'context-menu-functions 'code-cells--context-menu t)
      (font-lock-remove-keywords nil (code-cells--font-lock-keywords)))))
  (font-lock-flush))

;;;###autoload
(defun code-cells-mode-maybe ()
  "Turn on `code-cells-mode' if the buffer appears to contain cells.
This function is useful when added to a major mode hook."
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward code-cells-boundary-regexp 5000 t))
      (code-cells-mode)))

(easy-menu-define code-cells-mode-menu code-cells-mode-map
  "Menu for `code-cells-mode'."
  '("Notebook"
    ["Previous" code-cells-backward-cell
     :help "Go to the previous code cell boundary"]
    ["Next" code-cells-forward-cell
     :help "Go to the next code cell boundary"]
    ["Move Cell Up" code-cells-move-cell-up
     :help "Transpose the current code cell and the cell above"]
    ["Move Cell Down" code-cells-move-cell-down
     :help "Transpose the current code cell and the cell below"]
    "---"
    ["Cut" code-cells-kill
     :help "Cut (kill) the current code cell"]
    ["Copy" code-cells-copy
     :help "Copy the content of the current code cell"]
    ["Select" code-cells-mark-cell
     :help "Mark region of the current code cell"]
    ["Duplicate" code-cells-duplicate
     :help "Insert a copy of the current code cell below it"]
    ["Comment Out" code-cells-comment-or-uncomment
     :help "Comment or uncomment the current code cell"]
    "---"
    ["Evaluate" code-cells-eval
     :help "Evaluate current code cell (or region, if active) in a suitable shell"]
    ["Evaluate and Advance" code-cells-eval-and-step
     :help "Evaluate current code cell and go to the next one"]
    ["Evaluate Cells Above" code-cells-eval-above
     :help "Evaluate all code cells above the current one"]
    ["Evaluate Cells Below" code-cells-eval-below
     :help "Evaluate current code cell and all cells below it"]
    ["Evaluate Whole Buffer" code-cells-eval-whole-buffer
     :help "Evaluate entire buffer in a suitable shell"]))

(defun code-cells--context-menu (menu click)
  "Populate MENU with code cells commands."
  (if (use-region-p)
      (keymap-set menu "<code-cells-eval>"
       '(menu-item "Evaluate Region" code-cells-eval
                   :help "Evaluate the marked region"))
    (let ((bound (save-excursion
                   (mouse-set-point click)
                   (car (code-cells--bounds)))))
      (keymap-set menu "<code-cells-eval-buffer>"
       (if (eq bound (point-min))
           `(menu-item "Evaluate Whole Buffer" ,(lambda (_) (interactive "i")
                                                  (code-cells-eval-whole-buffer))
                       :help "Evaluate the entire buffer")
         `(menu-item "Evaluate Cells Above" ,(lambda (_) (interactive "i")
                                               (code-cells-eval-above bound))
                     :help "Evaluate code cells above click"))))
    (keymap-set menu "<code-cells-eval>"
     `(menu-item
       "Evaluate Cell"
       ,(lambda (e) (interactive "e")
          (save-excursion
            (mouse-set-point e)
            (call-interactively #'code-cells-eval)))
       :help "Evaluate the code cell at click")))
  (keymap-set-after menu "<select-region> <mark-code-cell>"
    `(menu-item "Cell" ,(lambda (e) (interactive "e")
                          (mark-thing-at-mouse e 'code-cell))
                :help "Mark the code cell at click for a subsequent cut/copy")
    'mark-whole-buffer)
  menu)

;;; * Jupyter notebook conversion

(defun code-cells--call-process (buffer command)
  "Pipe BUFFER through COMMAND, with output to the current buffer.
Returns the process exit code.  COMMAND is a list of strings, the
program name followed by arguments."
  (unless (executable-find (car command))
    (error "Can't find %s" (car command)))
  (let ((logfile (make-temp-file "emacs-code-cells-")))
    (unwind-protect
        (prog1
            (apply #'call-process-region nil nil (car command) nil
                   (list buffer logfile) nil
                   (cdr command))
          (with-temp-buffer
            (insert-file-contents logfile)
            (unless (zerop (buffer-size))
              (lwarn 'code-cells :warning
                     "Notebook conversion command %s said:\n%s"
                     command
                     (buffer-substring-no-properties
                      (point-min) (point-max))))))
      (delete-file logfile))))

(defun code-cells--guess-mode ()
  "Guess major mode associated to the current ipynb buffer."
  (goto-char (point-min))
  ;; Skip over the possibly huge "cells" section
  (search-forward "{")
  (while (not (equal (json-parse-buffer) "metadata"))
    (forward-sexp)
    (search-forward ","))
  (search-forward ":")
  (let* ((metadata (json-parse-buffer :object-type 'alist))
         (lang (let-alist metadata
                 (or .kernelspec.language
                     .jupytext.main_language)))
         (mode (intern (concat lang "-mode"))))
    (alist-get mode (bound-and-true-p major-mode-remap-alist) mode)))

;;;###autoload
(defun code-cells-convert-ipynb ()
  "Convert buffer from ipynb format to a regular script."
  (let* ((mode (funcall (caddr code-cells-convert-ipynb-style)))
         (exit (progn
                 (goto-char (point-min))
                 (code-cells--call-process t (cadr code-cells-convert-ipynb-style)))))
    (unless (zerop exit)
      (delete-region (point-min) (point))
      (error "Error converting notebook (exit code %s)" exit))
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (add-hook 'write-file-functions #'code-cells-write-ipynb 80 t)
    (when (fboundp mode)
      (funcall mode)
      (run-hooks (cadddr code-cells-convert-ipynb-style)))))

;;;###autoload
(defun code-cells-write-ipynb (&optional file)
  "Convert buffer to ipynb format and write to FILE.
Interactively, asks for the file name.  When called from Lisp,
FILE defaults to the current buffer file name."
  (interactive "F")
  (let* ((file (or file buffer-file-name))
         (temp (generate-new-buffer " *cells--call-process output*"))
         (exit (code-cells--call-process temp (car code-cells-convert-ipynb-style))))
    (unless (eq 0 exit)
        (error "Error converting notebook (exit code %s)" exit))
    (with-current-buffer temp
      (write-region nil nil file)
      (kill-buffer))
    (when (eq file buffer-file-name)
      (set-buffer-modified-p nil)
      (set-visited-file-modtime))
    'job-done))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-convert-ipynb))

(provide 'code-cells)
;;; * code-cells.el ends here
