EMACS ?= emacs
BATCH := $(EMACS) $(EFLAGS) -batch -q -no-site-file -L .

all: pangu-spacing.elc

README.md: make-readme-markdown.el
	emacs --script $< <pangu-spacing.el>$@ 2>/dev/null
make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el
.INTERMEDIATE: make-readme-markdown.el

clean:
	$(RM) *.elc

%.elc: %.el
	$(BATCH) --eval '(byte-compile-file "$<")'

test:
	$(BATCH) -L . -l test/test.el -f ert-run-tests-batch-and-exit

.PHONY: check clean test README.md
