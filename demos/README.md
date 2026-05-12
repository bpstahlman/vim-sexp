# Vim-Sexp Demos

These short demos show common vim-sexp editing operations. Each demo includes a
caption, a small Clojure fixture, and a generated GIF.

The GIFs are generated from asciinema casts so they can be rebuilt after
changing fixtures, captions, timing, or plugin behavior.

## Building

Required tools:

- `nvim`
- `asciinema`
- `agg`
- `gifsicle`

If `nvim` is installed as an AppImage on a system without FUSE, extract the
AppImage and point `nvim` or `DEMO_VIM` at the extracted executable:

```sh
nvim --appimage-extract
DEMO_VIM=/path/to/squashfs-root/AppRun make demos
```

By default, the demo runner adds `~/.local/share/nvim/lazy/nvim-treesitter` to
Neovim's runtimepath so `nvim -u NONE` can still load Treesitter parsers. If
your nvim-treesitter checkout lives elsewhere, set `DEMO_TS_RTP`:

```sh
DEMO_TS_RTP=/path/to/nvim-treesitter make demos
```

Generate all demos:

```sh
make demos
```

Generate one demo:

```sh
make demos/gifs/wrap-element.gif
```

Remove generated recordings and GIFs:

```sh
make clean-demos
```

## Visual Tour Demos

### Retarget Visual Selection

Command: `vaf`, `]e`, `]e`, `[e`

[![Move visual selection between sibling elements](gifs/select-adjacent.gif)](gifs/select-adjacent.gif)

[Open GIF directly](gifs/select-adjacent.gif)

### Wrap Element

Command: `<LocalLeader>w`

[![Wrap current element](gifs/wrap-element.gif)](gifs/wrap-element.gif)

[Open GIF directly](gifs/wrap-element.gif)

### Splice List

Command: `<LocalLeader>@`

[![Splice current list](gifs/splice-list.gif)](gifs/splice-list.gif)

[Open GIF directly](gifs/splice-list.gif)

### Capture Next Elements

Command: `<M-S-l>`

[![Capture following forms into the current list](gifs/capture-next.gif)](gifs/capture-next.gif)

[Open GIF directly](gifs/capture-next.gif)

### Swap Element

Command: `<M-l>`

[![Swap the current element forward through siblings](gifs/swap-element.gif)](gifs/swap-element.gif)

[Open GIF directly](gifs/swap-element.gif)

### Cleanup And Align Comments

Command: `==`

[![Clean indentation and align end-of-line comments](gifs/cleanup-align.gif)](gifs/cleanup-align.gif)

[Open GIF directly](gifs/cleanup-align.gif)
