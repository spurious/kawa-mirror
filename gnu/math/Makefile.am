## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@

.java.class:
	cd $(srcdir); $(JAVAC) $*.java

javadir=$(prefix)/java/kawa/lang


OBJ=\
  Numeric.class \
  RealNum.class \
  DFloNum.class \
  RatNum.class \
  MPN.class \
  IntNum.class

java_DATA=$(OBJ) $(EXCEPTIONS)

EXTRA_DIST = $(OBJ:.class=.java)

CLEANFILES=$(OBJ) $(EXCEPTIONS)

all: $(OBJ)

