## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang standard lib

.SUFFIXES : .java .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	cd $(srcdir); $(JAVAC) $(<F)

javadir=$(prefix)/java/kawa

OBJ = Shell.class

java_DATA=$(OBJ)

CLEANFILES=$(OBJ)

all: $(OBJ)



