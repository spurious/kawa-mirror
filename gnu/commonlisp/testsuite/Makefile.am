KAWALIB = ../../..
KAWA = KAWALIB=$(KAWALIB) $(KAWALIB)/kawa/kawa.sh
DOTEST = $(KAWA) --clisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

SOURCES = lang-test.lisp

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'
