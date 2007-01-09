KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
DOTEST = $(KAWA) --elisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

EXTRA_DIST = lang-test.el
CLEANFILES = *.log

check:
	$(DOTEST) -f "$(srcdir)/lang-test.el" -e '(test-report)'

all:
