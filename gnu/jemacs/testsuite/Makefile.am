KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
DOTEST = $(KAWA) --elisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

SOURCES = lang-test.el

all:

check_WITH_SWING:
	$(DOTEST) -f "$(srcdir)/lang-test.el" -e '(test-report)'

check_WITHOUT_SWING:
	@echo "(no checks, since configued without --with-swing)"

check: check_@SWING_SELECTED@
