cells.el
========

This package lets you efficiently navigate, edit and execute code
split into cells according to certain magic comments.  Such files can
be obtained, for instance, by converting a Jupyter notebook to a
script in the web interface or with

``` shell
jupyter nbconvert --to script <FILE.ipynb>
```

It's also worth mentioning [jupytext] and [ipynb-py-convert], which
can convert to and from notebook format.

Instead of defining every conceivable command relevant to code with
cells, this package provides a simple way to turn ordinary editing
commands into “cell-aware” ones.  The `cells-command` function takes
as argument a function that acts a region, and returns an anonymous
command that acts on the current cell.  Thus, one can redefine `C-c
C-c` in Python mode to evaluate the current cell by doing the
following:

``` elisp
(define-key
  python-mode-map
  (kbd "C-c C-c")
  (cells-command 'python-shell-send-region))
```

See below for a more substantial configuration example, which is
specifically for Jupyter mode but can be easily adapted to any other
mode or language with a REPL in Emacs.

Also available in this package are the following; see the
documentation for more details.

- `cells-do`: a macro that makes the current cells bounds accessible
  as variables.
- `cells-mark-cell`: mark current cell and activate region
- `cells-mode`: for now, just adds font locking to the cell
  boundaries.  Also provides a keymap where one can add any desired
  cell-related commands.

Keybindings for [emacs-jupyter]
-------------------------------

The following configuration snippet sets up cell navigation and
evaluation functions on buffers associated to a Jupyter kernel:

- Navigate cells with `M-p` and `M-n`.
- Mark, copy and kill cells by prefixing the usual command with `C-c`.
- Evaluate a cell with `C-c C-c`, but acts on region instead if it's
  active.

``` elisp
(with-eval-after-load "jupyter-repl"
  (require 'cells)
  (let ((map jupyter-repl-interaction-mode-map))
    (define-key map (kbd "M-p") 'cells-backward-cell)
    (define-key map (kbd "M-n") 'cells-forward-cell)
    (define-key map (kbd "C-c C-SPC") 'cells-mark-cell)
    (define-key map (kbd "C-c C-w") (cells-command 'kill-region :use-region))
    (define-key map (kbd "C-c M-w") (cells-command 'kill-ring-save :use-region))
    (define-key map (kbd "C-c C-c") (cells-command 'jupyter-eval-region :use-region :pulse))))
```

A hydra for [emacs-jupyter]
---------------------------

The following defines a handy [hydra] for Jupyter mode.  Activate it
with `M-x notebook-hydra/body RET` or bind that command to a key in
`jupyter-repl-interaction-mode-map`.

``` elisp
(defhydra notebook-hydra (:color red :hint nil)
  "
_j_/_k_: ↓/↑, _h_ome, _l_ast, _q_uit      \
Cell: _e_val, mark and e_x_ecute      \
Kernel: _r_estart, eval _a_bove, _z_: pop to
"
  ("h" beginning-of-buffer)
  ("l" (progn (end-of-buffer)
              (cells-backward-cell)))
  ("j" cells-forward-cell)
  ("k" cells-backward-cell)
  ("z" jupyter-repl-pop-to-buffer :color blue)
  ("x" (progn (cells-mark-cell)
              (call-interactively 'execute-extended-command)))
  ("SPC" cells-mark-cell)
  ("r" jupyter-repl-restart-kernel)
  ("a" (cells-do (pulse-momentary-highlight-region (point-min) beg)
                 (jupyter-eval-region (point-min) beg)))
  ("e" (cells-do (pulse-momentary-highlight-region beg end)
                 (jupyter-eval-region beg end)
                 (cells-forward-cell)))
  ("M-w" (cells-do (kill-ring-save beg end)))
  ("C-w" (cells-do (kill-region beg end)))
  ("q" nil :exit t))
```

The head `x` asks for a command like `M-x`, and executes it with the
current cell as active region.  Thus, for instance, typing `x
comment-dwim RET` in this hydra will comment out the current cell.

[jupytext]: https://github.com/mwouts/jupytext
[ipynb-py-convert]: https://github.com/kiwi0fruit/ipynb-py-convert/
[emacs-jupyter]: https://github.com/dzop/emacs-jupyter
[hydra]: https://github.com/abo-abo/hydra
