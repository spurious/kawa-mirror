## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang math standard lib

SUFFIXES = .java .class

JAVAC=javac -classpath @TOP_BUILDDIR@:$$CLASSPATH -d @TOP_BUILDDIR@

.java.class:
	cd $(srcdir); $(JAVAC) $*.java

javadir=$(prefix)/java/kawa

OBJ = Shell.class

java_DATA=$(OBJ)

EXTRA_DIST = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)



