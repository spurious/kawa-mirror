AUTOMAKE_OPTIONS = foreign

@Make_rules@

KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
CLEANFILES = tmp* *.log testing.zip *.class

java_sources = TestMisc.java

EXTRA_DIST = tab.xml.in maketab1.xql tab.html \
  multab.xql expectedmul.xml descend.xql expdescend.xml

all:

TestMisc.class:  TestMisc.java
	$(CLASSPATH_ENV) $(JAVAC) -d . $(JAVACFLAGS) $<
#$(JAVAC) -g -d . $<

check-misc: TestMisc.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) TestMisc

maketab1.class:  maketab1.xql
	$(KAWA) --main --xquery -C $<

# Should not run $(JAVA) when built --with-gcj, as that runs
# interpreter gij.  Instead, should compile to native.
check-maketab1:  maketab1.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) maketab1 >tmp2
	@if diff -b $(srcdir)/tab.html tmp2; then echo '# maketab1 test passes'; else echo FAIL maketab1 test; fi

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

check:  check-misc check-maketab1 check-multab check-descend
