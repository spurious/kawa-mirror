KAWALIB = ../../..
KAWA = KAWALIB=$(KAWALIB) $(KAWALIB)/kawa/kawa.sh
DOTEST = $(KAWA) --clisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'
