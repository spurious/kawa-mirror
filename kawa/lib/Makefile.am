## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVAC=javac -classpath ../..:$$CLASSPATH -d @TOP_BUILDDIR@
JAVA=java -classpath ../..:$$CLASSPATH

java.class:
	cd $(srcdir); $(JAVAC) $(<F)

.scm.class:
	cd ../..; $(JAVA) kawac $< kawa.lib.

javadir=$(prefix)/java/kawa/lib

OBJ=\
  do.class \
  when_unless.class

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all: $(OBJ)
