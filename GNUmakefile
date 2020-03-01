######################################################################
#                            GNU MAKEFILE                            #
######################################################################

# clear out suffix list
.SUFFIXES:

EMACS = emacs
ELC = $(EMACS) -Q -batch -L .

org-menu-objects = org-menu.elc org-menu-simple.elc org-menu-fl.elc

######################################################################
### Rules
######################################################################
.PHONY: org-menu clean
# define main goal of make
org-menu: $(org-menu-objects)

%.elc: %.el
	$(ELC) -f batch-byte-compile $<

clean:
	-rm *.elc