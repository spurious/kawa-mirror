## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@
JAVA=java -classpath @TOP_BUILDDIR@:$$CLASSPATH

java.class:
	cd $(srcdir); $(JAVAC) $(<F)

.scm.class:
	cd ../..; $(JAVA) kawac $< kawa.lib.

javadir=$(prefix)/java/kawa/lib

OBJ=\
  case.class \
  cond.class \
  delay.class \
  do.class \
  let.class \
  when_unless.class

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all: $(OBJ)
