EMACS ?= emacs
EFLAGS ?= -Q -L . -L ./test

ELS = navbar.el
ELS += navbarx-elscreen.el
ELS += navbarx-time.el
ELS += navbarx-version.el
ELCS = $(ELS:.el=.elc)

.PHONY: compile
compile: test/elscreen.elc $(ELCS)

%.elc: %.el
	$(EMACS) $(EFLAGS) --batch -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(ELCS)

.PHONY: distclean
distclean: clean
	rm -f test/elscreen.el

.PHONY: test
test: compile
	$(EMACS) $(EFLAGS) --batch -l test/navbar-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-interactive
test-interactive: compile
	$(EMACS) $(EFLAGS) -nw -l test/navbar-test.el --eval "(ert t)"
	$(EMACS) $(EFLAGS) -l test/navbar-test.el --eval "(ert t)"

.PHONY: test-all
test-all: test test-interactive

test/elscreen.el:
	curl -s https://raw.githubusercontent.com/papaeye/elscreen/develop/elscreen.el > $@
