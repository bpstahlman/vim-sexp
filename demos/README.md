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

## Initial Demos

### Wrap Element

Command: `<LocalLeader>w`

[![Wrap current element](gifs/wrap-element.gif)](gifs/wrap-element.gif)

[Open GIF directly](gifs/wrap-element.gif)

### Splice List

Command: `<LocalLeader>@`

[![Splice current list](gifs/splice-list.gif)](gifs/splice-list.gif)

[Open GIF directly](gifs/splice-list.gif)
