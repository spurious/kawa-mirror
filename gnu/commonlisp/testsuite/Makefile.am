KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
DOTEST = $(KAWA) --clisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

EXTRA_DIST = lang-test.lisp
CLEANFILES = *.log

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.lisp" -e '(test-report)'
