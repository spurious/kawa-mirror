## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVA=java -classpath ../..:$$CLASSPATH
JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

.scm.class:
	$(JAVA) kawac -d ../.. $< $(subdir).

javadir=$(prefix)/java/$(subdir)

OBJ=\
  std_syntax.class \
  when_unless.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.scm)

CLEANFILES=$(OBJ)

all: $(OBJ)
