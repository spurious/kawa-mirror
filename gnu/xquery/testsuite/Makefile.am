KAWALIB = ../../..
DISABLE_GCJ_COMPILED_KAWA = KAWALIB=$(KAWALIB) ../../../bin/kawa.sh
ENABLE_GCJ_COMPILED_KAWA = ../../../bin/kawa-bin
KAWA = $(@GCJ_COMPILED_SELECTED@_KAWA)
CLEANFILES = tmp? *.log testing.zip *.class

SOURCES = test1.xql expected1.xml tab.xml.in maketab1.xql tab.html

all:

test1.class:  test1.xql
	$(KAWA) --main --xquery -C $<

check-test1:  test1.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) test1 >tmp1
	diff -b $(srcdir)/expected1.xml tmp1

maketab1.class:  maketab1.xql
	$(KAWA) --main --xquery -C $<

check-maketab1:  maketab1.class
	CLASSPATH=.:$(KAWALIB) $(JAVA) maketab1 >tmp2
	diff -b $(srcdir)/tab.html tmp2

check:  check-test1 check-maketab1

