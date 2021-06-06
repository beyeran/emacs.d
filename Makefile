DOT_EMACS_PATH          := ../.emacs
EMACS_CONFIG_START_FILE := init.el

$(DOT_EMACS_PATH):
	@echo "(load \"~/.emacs.d/$(EMACS_CONFIG_START_FILE)\")" > $(DOT_EMACS_PATH)
	@echo "Dot Emacs written."

.PHONY: clean
clean:
	rm $(DOT_EMACS_PATH)
