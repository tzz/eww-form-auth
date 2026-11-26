EMACS ?= emacs
EL := eww-form-auth.el

.PHONY: compile lint test clean

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(EL)

lint: compile
	$(EMACS) -Q --batch -L . -l checkdoc --eval '(checkdoc-file "$(EL)")'

test:
	$(EMACS) -Q --batch -L . -L test -l test/eww-form-auth-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
