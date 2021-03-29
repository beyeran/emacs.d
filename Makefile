EMACS        = emacs
BUILD_SCRIPT = build.el
EVAL_LINE    = "(apb/tangle-files (concat dot-files-src \"*.org\"))"
OUTPUT_DIR   = ./elisp

build: $(OUTPUT_DIR)
	$(EMACS) --batch -l $(BUILD_SCRIPT) --eval $(EVAL_LINE)

$(OUTPUT_DIR):
	mkdir -p $@

clean:
	rm -rf $(OUTPUT_DIR)

all: build
