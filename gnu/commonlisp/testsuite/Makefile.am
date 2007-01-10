KAWALIB = ../../..
KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa
DOTEST = $(KAWA) --clisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

EXTRA_DIST = lang-test.lisp
CLEANFILES = *.log

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'
