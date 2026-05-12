#!/bin/sh
set -eu

if [ "$#" -lt 7 ] || [ "$#" -gt 8 ]; then
    echo "usage: $0 NAME FIXTURE LINE COL KEYS DISPLAY_KEYS CAPTION [SETUP]" >&2
    exit 2
fi

name=$1
fixture=$2
line=$3
col=$4
keys=$5
display_keys=$6
caption=$7
setup=${8:-}

cast="demos/casts/$name.cast"
render_cast="demos/casts/$name.render.cast"
raw_gif="demos/gifs/$name.raw.gif"
gif="demos/gifs/$name.gif"
vim_cmd=${DEMO_VIM:-nvim}
ts_rtp=${DEMO_TS_RTP:-$HOME/.local/share/nvim/lazy/nvim-treesitter}

mkdir -p demos/casts demos/gifs .cache/asciinema

rm -f "$cast" "$render_cast" "$raw_gif" "$gif"

ASCIINEMA_CONFIG_HOME=.cache/asciinema \
DEMO_NAME="$name" \
DEMO_FIXTURE="demos/fixtures/$fixture" \
DEMO_LINE="$line" \
DEMO_COL="$col" \
DEMO_KEYS="$keys" \
DEMO_DISPLAY_KEYS="$display_keys" \
DEMO_CAPTION="$caption" \
DEMO_SETUP="$setup" \
DEMO_TS_RTP="$ts_rtp" \
asciinema rec --quiet --overwrite --command "env TERM=xterm-256color $vim_cmd -Nu NONE -n -i NONE -S demos/scripts/demo-driver.vim" "$cast"

if grep -Eq 'Error|E[0-9]+:|failed|aborted|Assertion|Can.t' "$cast"; then
    echo "recording failed; see $cast" >&2
    exit 1
fi

python3 - "$cast" "$render_cast" <<'PY'
import json
import sys
from pathlib import Path

src = Path(sys.argv[1])
dst = Path(sys.argv[2])
lines = src.read_text().splitlines()
header = json.loads(lines[0])
events = [json.loads(line) for line in lines[1:]]

start = next((i for i, event in enumerate(events)
              if event[1] == "o" and "Press:" in event[2]), 0)

trimmed = []
for event in events[start:]:
    data = event[2]
    if event[1] == "o" and ("\x1b[?1049l" in data or "\x1b[?1l" in data):
        break
    trimmed.append(event)

if not trimmed:
    raise SystemExit("no renderable events found")

offset = trimmed[0][0]
with dst.open("w") as f:
    print(json.dumps(header, separators=(",", ":")), file=f)
    for event in trimmed:
        event[0] = max(0, event[0] - offset)
        print(json.dumps(event, separators=(",", ":")), file=f)
PY

agg --theme github-light --font-size 16 --last-frame-duration 1.6 "$render_cast" "$raw_gif"
gifsicle -O3 --colors 64 "$raw_gif" -o "$gif"
rm -f "$render_cast" "$raw_gif"
