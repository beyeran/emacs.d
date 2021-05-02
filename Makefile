DOT_EMACS_PATH          := ../.emacs
EMACS_CONFIG_DIR        := elisp
EMACS_CONFIG_START_FILE := apb-main.el

$(DOT_EMACS_PATH):
	@echo "(load \"~/.emacs.d/$(EMACS_CONFIG_DIR)/$(EMACS_CONFIG_START_FILE)\")" > $(DOT_EMACS_PATH)
	@echo "Dot Emacs written."

.PHONY: clean
clean:
	rm $(DOT_EMACS_PATH)
