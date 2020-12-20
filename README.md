cells.el
========

This package lets you efficiently navigate, edit and execute code
split into cells according to certain magic comments.

![Screenshot](https://user-images.githubusercontent.com/6500902/102713720-8c52dd80-42ca-11eb-8ea6-4e96a814be2b.png)

If you have [Jupytext] or [Pandoc] installed, you can also directly
open ipynb notebook files in Emacs.  They will be automatically
converted to a script for editing, and converted back to notebook
format when saving.

By default, three styles of comments are recognized as cell boundaries:

```
# In[<number>]:

# %% Optional title

#* Optional title
```

The first is what you get by exporting a notebook to a script on
Jupyter's web interface or with the command `jupyter nbconvert --to
python <FILE.ipynb>`.  The second style is compatible with Jupytext,
among several other tools.  The third is in the spirit of Emacs's
outline mode.

Editing commands
----------------

Instead of defining every conceivable command relevant to code with
cells, this package provides a simple way to turn ordinary editing
commands into “cell-aware” ones.  The `cells-command` function takes
as argument a function that acts on a region, and returns an anonymous
command that acts on the current cell.  Thus, one can redefine `C-c
C-c` in Python mode to evaluate the current cell by doing the
following:

``` elisp
(define-key python-mode-map
            (kbd "C-c C-c")
            (cells-command 'python-shell-send-region))
```
There is also a `cells-do` macro, which evaluates its body with the
current cell bounds accessible as variables.  See the documentation
for details.

Besides that, only three editing commands are provided:

- `cells-forward-cell` and `cells-backward-cell` jump to the
  next/previous cell header.
- `cells-mark-cell` marks the current cell and activates the region.

Everything else is left up for the user to define with the mechanism
explained above.  See below for a more substantial configuration
example, which is specific for Jupyter mode but can be easily adapted
to any other mode or language with a REPL in Emacs.

Minor mode
----------

A minor-mode, `cells-mode`, provides the following things:

- Font locking for cell boundaries.
- The `cells-mode-map` keymap, a good place for your very own cell
  commands.
- Outline mode integration: cell headers have outline level determined
  by the number of percent signs or asterisks; within a cell, the
  outline level is as determined by the major mode.  This provides
  code folding and hierarchical navigation, among other things, when
  `outline-minor-mode` is active.

Handling Jupyter notebook files
-------------------------------

With this package, you can edit Jupyter notebook (`*.ipynb`) files as
if they were normal plain-text scripts.  Converting to and from the
JSON-based ipynb format is done by an external tool, [Jupytext] by
default, which needs to be installed separately.

**Important notice:** The automatic format conversion on save hasn't
been thoroughly tested.  In particular, it (probably) doesn't handle
backups in the expected way.  Do not rely on its correctness!

Note also that the result cells of ipynb files are not retained in the
conversion to script format.  This means that opening and then saving
an ipynb file clears all cell outputs.

Within an ipynb buffer, you can use the regular `write-file` command
(`C-x C-w`) to save a copy in script format, as displayed on the
screen.  Moreover, from any script file with cell separators
understood by Jupytext, you can call `cells-write-ipynb` to save a
copy in notebook format.

Configuration examples
----------------------

### Keybindings for cells-mode

The following configuration snippet sets up cell navigation and
evaluation functions when `cells-modes` is enabled.  Just adapt it to
your liking.

- Navigate cells with `M-p` and `M-n`.
- Mark, copy and kill cells by prefixing the usual command with `C-c`.
- Evaluate a cell with the same key combinations that would otherwise
  evaluate the region (but still acting on the region instead if it is
  active, as stipulated by the `:use-region` flag).  This is done for
  a couple of different major modes via key remaps, which see.

``` elisp
(require 'cells)
(let ((map cells-mode-map))
  (define-key map (kbd "M-p") 'cells-backward-cell)
  (define-key map (kbd "M-n") 'cells-forward-cell)
  (define-key map (kbd "C-c C-SPC") 'cells-mark-cell)
  (define-key map (kbd "C-c C-w") (cells-command 'kill-region :use-region))
  (define-key map (kbd "C-c M-w") (cells-command 'kill-ring-save :use-region))
  (define-key map [remap python-shell-send-region]
                  (cells-command 'python-shell-send-region :use-region :pulse))
  (define-key map [remap jupyter-eval-line-or-region]
                  (cells-command 'jupyter-eval-region :use-region :pulse))
  (define-key map [remap cider-eval-region]
                  (cells-command 'cider-eval-region :use-region :pulse)))
```

### A hydra for [emacs-jupyter]

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

The head `x` asks for a command just like `M-x`, and executes it with
the current cell as active region.  Thus, for instance, typing `x
comment-dwim RET` in this hydra will comment out the current cell.

Note that since `defhydra` is a macro and wraps the definition of a
key in an interactive lambda when it's a sexp, we need to use
`cells-do` instead of `cells-command` above.

### Tweaking the ipynb conversion

The default settings translate notebooks to Jupytext's “percent”
format, but this can be changed.  To convert to markdown, use to
following:

``` elisp
(setq cells-ipynb-convert-style '(("jupytext" "--to" "ipynb" "--from" "markdown")
                                  ("jupytext" "--to" "markdown" "--from" "ipynb")
                                  markdown-mode))
```

To edit ipynb files as org documents, try using [Pandoc] like this:

```elisp
(setq cells-ipynb-convert-style '(("pandoc" "--to" "ipynb" "--from" "org")
                                  ("pandoc" "--to" "org" "--from" "ipynb")
                                  org-mode))
```

A good reason to stick with Jupytext, though, is that it offers
round-trip stability: if you save a script and then revert the buffer,
the buffer shouldn't change.  With other tools, you may get some
surprises.

Alternatives
------------

[python-cells.el](https://github.com/thisch/python-cell.el) provides
similar cell editing commands.  It seems to be limited to Python, and
is perhaps simpler to set up but less flexible.

With Jupytext's [paired notebook mode](https://jupytext.readthedocs.io/en/latest/paired-notebooks.html)
it is possible to keep a notebook open in JupyterLab and simultaneously
edit a script version in an external text editor.

[jupytext]: https://github.com/mwouts/jupytext
[pandoc]: https://pandoc.org/
[emacs-jupyter]: https://github.com/dzop/emacs-jupyter
[hydra]: https://github.com/abo-abo/hydra
