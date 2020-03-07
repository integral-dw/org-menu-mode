######################################################################
#                            GNU MAKEFILE                            #
######################################################################

# clear out suffix list
.SUFFIXES:

EMACS = emacs
ELC = $(EMACS) -Q -batch -L .
EFLAGS = -l tests/om-listener.el

org-menu-objects = org-menu.elc org-menu-simple.elc org-menu-fl.elc

######################################################################
### Rules
######################################################################
.PHONY: org-menu clean test-run
# define main goal of make
org-menu: $(org-menu-objects)

%.elc: %.el
	$(ELC) -f batch-byte-compile $<

test-run: org-menu
	$(EMACS) -Q -L . -l tests/minimal-setup.el $(EFLAGS) tests/sample.org

clean:
	-rm *.elc
