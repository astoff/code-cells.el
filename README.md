code-cells.el
=============

[![MELPA](https://melpa.org/packages/code-cells-badge.svg)](https://melpa.org/#/code-cells)

This package lets you efficiently navigate, edit and execute code
split into cells according to certain magic comments.  If you have
[Jupytext] or [Pandoc] installed, you can also open ipynb notebook
files directly in Emacs.  They will be automatically converted to a
script for editing, and converted back to notebook format when saving.

![Screenshot](https://user-images.githubusercontent.com/6500902/104836187-652d0300-58ac-11eb-836b-e91baa545fb0.png)

By default, three styles of comments are recognized as cell boundaries:

```
# In[<number>]:

# %% Optional title

# * Optional title
```

The first is what you get by exporting a notebook to a script on
Jupyter's web interface or with the command `jupyter nbconvert`.  The
second style is compatible with Jupytext, among several other tools.
The third is in the spirit of Emacs's outline mode.  Further percent
signs or asterisks signify nested cells.

Minor mode
----------

The `code-cells-mode` minor mode provides the following things:

- Fontification of cell boundaries.
- Keybindings for the cell navigation and evaluation commands, under
  the `C-c %` prefix.
- Outline mode integration: cell headers have outline level determined
  by the number of percent signs or asterisks; within a cell, outline
  headings are as determined by the major mode, but they are demoted
  by an amount corresponding to the level of the containing cell.
  This provides code folding and hierarchical navigation, among other
  things, when `outline-minor-mode` is active.

`code-cells-mode` is automatically activated when opening an ipynb
file, but of course you can activate it in any other buffer, either
manually or through some hook.

Editing commands
----------------

The following editing and navigation commands are provided.  Their
keybindings in `code-cells-mode-map` are also shown.  Note, however,
that these commands do not require the minor mode to be active.

- <kbd>C-c % e</kbd>: `code-cells-eval`
- <kbd>C-c % b</kbd>: `code-cells-backward-cell`
- <kbd>C-c % f</kbd>: `code-cells-forward-cell`
- <kbd>C-c % ;</kbd>: `code-cells-comment-or-uncomment`
- <kbd>C-c % @</kbd>: `code-cells-mark-cell`

The `code-cells-eval` command sends the current cell to a suitable
REPL, chosen according to the current major and minor modes.  The
exact behavior is controlled by the `code-cells-eval-region-commands`
variable, which can be customized to suit your needs.

You might prefer shorter keybindings for some of these commands.  One
sensible possibility is to use `C-c C-c` to evaluate and `M-p`/`M-n`
to navigate cells.  This can be achieved with the following
configuration:

``` elisp
(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))
```

Speed keys
----------

Similarly to org-mode's [speed keys], the `code-cells-speed-key`
function returns a key definition that only acts when the point is at
the beginning of a cell boundary.  Since this is usually not an
interesting place to insert text, you can assign short keybindings
there.

No speed keys are set up by default.  A sample configuration is as
follows:

``` elisp
(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key (lambda ()
                                                        "Show/hide current cell"
                                                        (interactive)
                                                        (outline-minor-mode)
                                                        (if (outline-invisible-p (line-end-position))
                                                            (outline-show-subtree)
                                                          (outline-hide-subtree)))))))
```

Handling Jupyter notebook files
-------------------------------

With this package, you can edit Jupyter notebook (`*.ipynb`) files as
if they were normal plain-text scripts.  Converting to and from the
JSON-based ipynb format is done by an external tool, [Jupytext] by
default, which needs to be installed separately.

Note that the result cells of ipynb files are not retained in the
conversion to script format.  This means that opening and then saving
an ipynb file clears all cell outputs.

While editing a converted ipynb buffer, you can use the regular
`write-file` command (`C-x C-w`) to save a copy in script format, as
displayed on the screen.  Moreover, from any script file with cell
separators understood by Jupytext, you can call
`code-cells-write-ipynb` to save a copy in notebook format.

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
be limited to Python code.

With Jupytext's [paired notebook mode](https://jupytext.readthedocs.io/en/latest/paired-notebooks.html)
it is possible to keep a notebook open in JupyterLab and simultaneously
edit a script version in an external text editor.

The [EIN] package allows to open ipynb files directly in Emacs with an
UI similar to Jupyter notebooks.  Note that EIN also registers major
modes for ipynb files; when installing both packages at the same time,
you may need to adjust your `auto-mode-alist` manually.

[ein]: https://github.com/dickmao/emacs-ipython-notebook
[emacs-jupyter]: https://github.com/dzop/emacs-jupyter
[jupytext]: https://github.com/mwouts/jupytext
[pandoc]: https://pandoc.org/
[python-cell.el]: https://github.com/thisch/python-cell.el
[speed keys]: https://orgmode.org/manual/Speed-Keys.html
