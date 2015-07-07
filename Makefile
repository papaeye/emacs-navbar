EMACS ?= emacs
EFLAGS ?= -Q -L . -L ./test

CASK ?= cask

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
	rm -rf dist

.PHONY: distclean
distclean: clean
	rm -f test/elscreen.el
	rm -rf .cask

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
	curl -sSL https://github.com/emacs-jp/elscreen/raw/master/elscreen.el > $@

.PHONY: package
package:
	$(CASK) install
	$(CASK) package

.PHONY: travis
test-travis: package
	$(CASK) exec $(EMACS) --batch -f package-initialize --eval '(package-install-file (car (file-expand-wildcards "$(shell pwd)/dist/navbar-*.tar")))'
	$(CASK) exec $(EMACS) --batch -f package-initialize -l test/navbar-test.el -f ert-run-tests-batch-and-exit
	$(CASK) exec $(EMACS) --batch -f package-initialize -l undercover --eval '(undercover "navbar.el")' -l navbar.el -l test/navbar-test.el -f ert-run-tests-batch-and-exit
