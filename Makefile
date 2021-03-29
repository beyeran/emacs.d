EMACS        = emacs
BUILD_SCRIPT = build.el
EVAL_LINE    = "(apb/tangle-files (concat dot-files-src \"*.org\"))"

build:
	$(EMACS) --batch -l $(BUILD_SCRIPT) --eval $(EVAL_LINE)
