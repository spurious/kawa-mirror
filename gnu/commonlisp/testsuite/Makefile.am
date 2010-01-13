KAWALIB = ../../..
KAWA = ../../../bin/kawa.sh
DOTEST = $(KAWA) --clisp\
 -f "$(KAWALIB)/testsuite/testing.zip" $(OPTIONS)

EXTRA_DIST = lang-test.lisp
CLEANFILES = *.log

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'
