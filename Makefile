.PHONY: demos clean-demos check-demo-tools

DEMOS := wrap-element splice-list
GIFS := $(addprefix demos/gifs/,$(addsuffix .gif,$(DEMOS)))
DEMO_FIXTURES := $(wildcard demos/fixtures/*.clj)

demos: $(GIFS)

check-demo-tools:
	@command -v nvim >/dev/null || { echo "missing nvim"; exit 1; }
	@command -v asciinema >/dev/null || { echo "missing asciinema"; exit 1; }
	@command -v agg >/dev/null || { echo "missing agg"; exit 1; }
	@command -v gifsicle >/dev/null || { echo "missing gifsicle"; exit 1; }

demos/gifs/%.gif: Makefile demos/manifest.tsv demos/scripts/demo-driver.vim demos/scripts/record-demo.sh $(DEMO_FIXTURES) | check-demo-tools
	@awk -F '\t' -v demo="$*" '$$1 == demo { found = 1; \
		cmd = "demos/scripts/record-demo.sh " \
			"\"" $$1 "\" " \
			"\"" $$2 "\" " \
			"\"" $$3 "\" " \
			"\"" $$4 "\" " \
			"\"" $$5 "\" " \
			"\"" $$6 "\" " \
			"\"" $$7 "\""; \
		exit system(cmd) } END { if (!found) { print "demo not found: " demo > "/dev/stderr"; exit 1 } }' demos/manifest.tsv

clean-demos:
	rm -f demos/casts/*.cast demos/gifs/*.gif
