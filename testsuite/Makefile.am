## Process this with automake to create Makefile.in

JAVA=java
JAVAFLAGS=-classpath ..:$$CLASSPATH
KAWATEST = $(JAVA) $(JAVAFLAGS) kawa -f "$(srcdir)/testing.scm"

all:

check:  check-scm check-num

check-scm:
	rm -rf tmp?
	$(KAWATEST) -e '(define this-file-name "$(srcdir)/test.scm")' \
	  -f $(srcdir)/test.scm -e '(test-report)'

check-num:
	$(KAWATEST) -f "$(srcdir)/num-test.scm" -e '(test-report)'

CLEANFILES = tmp?
