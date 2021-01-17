code-cells.el
=============

[![MELPA](https://melpa.org/packages/code-cells-badge.svg)](https://melpa.org/#/code-cells)

This package lets you efficiently navigate, edit and execute code
split into cells according to certain magic comments.  If you have
[Jupytext] or [Pandoc] installed, you can also open ipynb notebook
files directly in Emacs.  They will be automatically converted to a
script for editing, and converted back to notebook format when saving.

![Screenshot](https://user-images.githubusercontent.com/6500902/102713720-8c52dd80-42ca-11eb-8ea6-4e96a814be2b.png)

By default, three styles of comments are recognized as cell boundaries:

```
# In[<number>]:

# %% Optional title

#* Optional title
```

The first is what you get by exporting a notebook to a script on
Jupyter's web interface or with the command `jupyter nbconvert`.  The
second style is compatible with Jupytext, among several other tools.
The third is in the spirit of Emacs's outline mode.  Further percent
signs or asterisks signify nested cells.  In fact, `code-cells-mode`
doubles as a general-purpose enhancement of `outline-minor-mode`; see
below for details.

Editing commands
----------------

Instead of defining every conceivable command relevant to code with
cells, this package provides a simple way to turn ordinary editing
commands into “cell-aware” ones.  The `code-cells-command` function
takes as argument a function that acts on a region, and returns an
anonymous command that acts on the current cell.  Thus, one can
redefine `C-c C-c` in Python mode to evaluate the current cell by
doing the following:

``` elisp
(define-key python-mode-map
            (kbd "C-c C-c")
            (code-cells-command 'python-shell-send-region))
```

There is also a `code-cells-do` macro, which evaluates its body with
the current cell bounds accessible as variables.  See the
documentation for details.

Besides that, only three editing commands are provided:

- `code-cells-forward-cell` and `code-cells-backward-cell` jump to the
  next/previous cell header.
- `code-cells-mark-cell` marks the current cell and activates the
  region.

Everything else is left up for the user to define with the mechanism
explained above.  See below for more substantial configuration
examples, some specific to Jupyter and others applicable to any mode
or language with a REPL in Emacs.

Minor mode
----------

A minor-mode, `code-cells-mode`, provides the following things:

- Font locking for cell boundaries.
- The `code-cells-mode-map` keymap, a good place for your very own
  cell commands.
- Outline mode integration: cell headers have outline level determined
  by the number of percent signs or asterisks; within a cell, outline
  headings are as determined by the major mode, but they are demoted
  by an amount corresponding to the level of the containing cell.
  This provides code folding and hierarchical navigation, among other
  things, when `outline-minor-mode` is active.

Many major modes provide an outline hierarchy based on code structure,
and some people prefer to replace this with a hierarchy based on
sectioning comments.  With `code-cells-mode` you get both things at
the same time.  This may be useful even for code that is not organized
as a notebook.

Handling Jupyter notebook files
-------------------------------

With this package, you can edit Jupyter notebook (`*.ipynb`) files as
if they were normal plain-text scripts.  Converting to and from the
JSON-based ipynb format is done by an external tool, [Jupytext] by
default, which needs to be installed separately.

**Important notice:** The automatic format conversion on save hasn't
been thoroughly tested.  In particular, it might not handle backups in
the expected way.  Do not rely on its correctness!

Note also that the result cells of ipynb files are not retained in the
conversion to script format.  This means that opening and then saving
an ipynb file clears all cell outputs.

With a converted ipynb buffer, you can use the regular `write-file`
command (`C-x C-w`) to save a copy in script format, as displayed on
the screen.  Moreover, from any script file with cell separators
understood by Jupytext, you can call `code-cells-write-ipynb` to save
a copy in notebook format.

Configuration examples
----------------------

### Keybindings for code-cells-mode

The following configuration snippet sets up cell navigation and
evaluation functions when `code-cells-mode` is enabled.  Just adapt it
to your liking.

- Navigate cells with `M-p` and `M-n`.
- Mark, copy and kill cells by prefixing the usual command with `C-c`.
- Evaluate a cell with the same key combinations that would otherwise
  evaluate the region (but still acting on the region instead if it is
  active, as stipulated by the `:use-region` flag).  This is done for
  a couple of different modes via key remaps, which see.

``` elisp
(require 'cells)
(let ((map code-cells-mode-map))
  (define-key map (kbd "M-p") 'cells-backward-cell)
  (define-key map (kbd "M-n") 'cells-forward-cell)
  (define-key map (kbd "C-c C-SPC") 'cells-mark-cell)
  (define-key map (kbd "C-c C-w") (code-cells-command 'kill-region :use-region))
  (define-key map (kbd "C-c M-w") (code-cells-command 'kill-ring-save :use-region))
  (define-key map [remap python-shell-send-region]
                  (code-cells-command 'python-shell-send-region :use-region :pulse))
  (define-key map [remap jupyter-eval-line-or-region]
                  (code-cells-command 'jupyter-eval-region :use-region :pulse))
  (define-key map [remap cider-eval-region]
                  (code-cells-command 'cider-eval-region :use-region :pulse)))
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
              (code-cells-backward-cell)))
  ("j" code-cells-forward-cell)
  ("k" code-cells-backward-cell)
  ("z" jupyter-repl-pop-to-buffer :color blue)
  ("x" (progn (code-cells-mark-cell)
              (call-interactively 'execute-extended-command)))
  ("C-SPC" code-cells-mark-cell)
  ("r" jupyter-repl-restart-kernel)
  ("a" (code-cells-do (pulse-momentary-highlight-region (point-min) start)
                      (jupyter-eval-region (point-min) start)))
  ("e" (code-cells-do (pulse-momentary-highlight-region start end)
                      (jupyter-eval-region start end)
                      (code-cells-forward-cell)))
  ("M-w" (code-cells-do (kill-ring-save start end)))
  ("C-w" (code-cells-do (kill-region start end)))
  ("q" nil :exit t))
```

The head `x` asks for a command just like `M-x`, and executes it with
the current cell as active region.  Thus, for instance, typing `x
comment-dwim RET` in this hydra will comment out the current cell.

Note that since `defhydra` is a macro and wraps the definition of a
key in an interactive lambda when it is a sexp, we need to use
`code-cells-do` instead of `code-cells-command` above.

### Speed keys

Similarly to org-mode's [speed keys](https://orgmode.org/manual/Speed-Keys.html),
the `code-cells-speed-key` function returns a key definition that only acts
when the point is at the beginning of a cell boundary.  Since this is
usually not an interesting place to insert text, you can assign short
keybindings there.  A sample configuration is as follows:

``` elisp
(require 'cells)
(let ((map code-cells-mode-map))
  (define-key map "n" (code-cells-speed-key 'cells-forward-cell))
  (define-key map "p" (code-cells-speed-key 'cells-backward-cell))
  (define-key map "e" (code-cells-speed-key (code-cells-command 'your-favorite-eval-region)))
  (define-key map (kbd "TAB") (code-cells-speed-key (lambda ()
                                                      "Show/hide current cell"
                                                      (interactive)
                                                      (outline-minor-mode)
                                                      (if (outline-invisible-p (line-end-position))
                                                          (outline-show-subtree)
                                                        (outline-hide-subtree))))))
```

### Tweaking the ipynb conversion

If relegating markdown cells to comment blocks offends your literate
programmer sensibilities, try including the following in the YAML
header of a converted notebook (and then save and revert it).  It will
cause text cells to be displayed as multiline comments.

``` yaml
jupyter:
  jupytext:
    cell_markers: '"""'
```

It is also possible to convert notebooks to markdown or org format.
For markdown, use the following:

``` elisp
(setq code-cells-convert-ipynb-style '(("jupytext" "--to" "ipynb" "--from" "markdown")
                                       ("jupytext" "--to" "markdown" "--from" "ipynb")
                                       markdown-mode))
```

To edit ipynb files as org documents, try using [Pandoc] with the
configuration below.  In combination with org-babel, this can provide
a more notebook-like experience, with interspersed code and results.

```elisp
(setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
                                       ("pandoc" "--to" "org" "--from" "ipynb")
                                       org-mode))
```

A good reason to stick with Jupytext, though, is that it offers
round-trip consistency: if you save a script and then revert the
buffer, the buffer shouldn't change.  With other tools, you may get
some surprises.

Alternatives
------------

[python-cell.el] provides similar cell editing commands.  It seems to
be limited to Python, and is perhaps simpler to set up but less
flexible.

With Jupytext's [paired notebook mode](https://jupytext.readthedocs.io/en/latest/paired-notebooks.html)
it is possible to keep a notebook open in JupyterLab and simultaneously
edit a script version in an external text editor.

The [EIN] package allows to open ipynb files directly in Emacs with an
UI similar to Jupyter notebooks.  Note that EIN also registers major
modes for ipynb files; when installing both packages at the same time,
you may need to adjust your `auto-mode-alist` manually.

[ein]: https://github.com/dickmao/emacs-ipython-notebook
[emacs-jupyter]: https://github.com/dzop/emacs-jupyter
[hydra]: https://github.com/abo-abo/hydra
[jupytext]: https://github.com/mwouts/jupytext
[pandoc]: https://pandoc.org/
[python-cell.el]: https://github.com/thisch/python-cell.el
