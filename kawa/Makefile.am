## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang math standard lib

SUFFIXES = .java .class

JAVAC=javac -classpath ..:$(srcdir)/..:$$CLASSPATH

.java.class:
	$(JAVAC) -d .. $<

javadir=$(prefix)/java/kawa

OBJ = Shell.class

java_DATA=$(OBJ)

EXTRA_DIST = $(OBJ:.class=.java)

CLEANFILES=$(OBJ)

all: $(OBJ)



