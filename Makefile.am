## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = codegen kawa

.SUFFIXES : .java .class

JAVAC=javac -classpath .:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java

OBJ = kawa.class

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all: $(OBJ)



