include $(top_srcdir)/Make-rules

KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
EXTRA_CLEAN = tmp* *.log testing.zip *.class

java_sources = TestMisc.java TestSuite.java

EXTRA_DIST = tab.xml.in maketab1.xql tab.html \
  multab.xql expectedmul.xml descend.xql expdescend.xml \
  bib.xml.in use-cases.xml book1.xml.in reviews.xml.in \
  bids.xml.in users.xml.in items.xml.in format-users.xsl exp-format-users.html

all:

TestMisc.class:  TestMisc.java
	$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) $<

TestSuite.class:  TestSuite.java
	$(CLASSPATH_ENV) $(JAVAC) -d $(JAVAROOT) $(JAVACFLAGS) $<

check-misc: TestMisc.class
	CLASSPATH=$(KAWALIB) $(JAVA) gnu.xquery.testsuite.TestMisc

check-suite:  TestSuite.class
	CLASSPATH=$(KAWALIB) \
	  $(JAVA) gnu.xquery.testsuite.TestSuite $(srcdir)/use-cases.xml

maketab1.class:  maketab1.xql
	$(KAWA) --main --xquery -C $<

# Should not run $(JAVA) when built --with-gcj, as that runs
# interpreter gij.  Instead, should compile to native.
check-maketab1:  maketab1.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) maketab1 >tmp2
	@if diff -b $(srcdir)/tab.html tmp2; then echo '# maketab1 test passes'; else echo FAIL maketab1 test; fi

# FIXME - this does not do the correct thing
check-desc:
	$(KAWA) --xquery -e 'document("gnu/xquery/testsuite/bib.xml")//*[1]'

multab.class:  multab.xql
	$(KAWA) --main --xquery -C $<

check-multab:  multab.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) multab >tmpmul
	@if diff -b $(srcdir)/expectedmul.xml tmpmul; then echo '# multab test passes'; else echo FAIL multab test; fi

descend.class:  descend.xql
	$(KAWA) --main --xquery -C $<

check-descend:  descend.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) descend >tmpdesc
	@if diff -b $(srcdir)/expdescend.xml tmpdesc; then echo '# descend test passes'; else echo FAIL descend test; fi

check-format-users:
	$(KAWA) --xslt $(srcdir)/format-users.xsl users.xml \
	  |sed 's/^ *//'>tmp-format-users.html
	@if diff -b -B $(srcdir)/exp-format-users.html tmp-format-users.html; then echo '#  format-users test passes'; else echo FAIL format-users test; fi

check:  check-misc check-suite check-maketab1 check-multab check-descend \
	  check-format-users
