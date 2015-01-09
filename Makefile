EMACS ?= emacs

ELS = navbar.el
ELCS = $(ELS:.el=.elc)

.PHONY: compile
compile: $(ELCS)

%.elc: %.el
	$(EMACS) --batch -Q -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(ELCS)

.PHONY: test
test: compile
	$(EMACS) --batch -Q -L . -l test/navbar-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive: compile
	$(EMACS) -nw -Q -L . -l test/navbar-test.el --eval "(ert t)"
	$(EMACS) -Q -L . -l test/navbar-test.el --eval "(ert t)"

.PHONY: test-all
test-all: test test-interactive
