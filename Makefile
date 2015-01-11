EMACS ?= emacs

ELS = navbar.el
ELCS = $(ELS:.el=.elc)

.PHONY: compile
compile: $(ELCS)

%.elc: %.el
	$(EMACS) --batch -Q -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(ELCS) test/elscreen.*

.PHONY: test
test: compile test/elscreen.elc
	$(EMACS) --batch -Q -L . -L ./test -l test/navbar-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive: compile test/elscreen.elc
	$(EMACS) -nw -Q -L . -L ./test -l test/navbar-test.el --eval "(ert t)"
	$(EMACS) -Q -L . -L ./test -l test/navbar-test.el --eval "(ert t)"

.PHONY: test-all
test-all: test test-interactive

.PHONY: test/elscreen.el
test/elscreen.el:
	curl -s https://raw.githubusercontent.com/papaeye/elscreen/develop/elscreen.el > $@
