KAWALIB = ../../..
KAWA = KAWALIB=$(KAWALIB) $(KAWALIB)/kawa/kawa.sh
DOTEST = $(KAWA) --elisp\
 -e '(load "$(KAWALIB)/testsuite/testing.zip")' $(OPTIONS)

all:

check:
	$(DOTEST) -f "$(srcdir)/lang-test.el" -e '(test-report)'
