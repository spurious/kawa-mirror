## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVA=java -classpath ../..:$$CLASSPATH
JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

.scm.class:
	$(JAVA) kawac -d ../.. $< kawa.lib.

javadir=$(prefix)/java/kawa/lib

OBJ=\
  case.class \
  cond.class \
  delay.class \
  do.class \
  let.class \
  when_unless.class

java_DATA=$(OBJ)

EXTRA_DIST = $(OBJ:.class=.scm)

CLEANFILES=$(OBJ)

all: $(OBJ)
