## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = math lang standard lib

SUFFIXES = .java .class

bin_SCRIPTS = kawa

# Sufix for batch files.
BAT=

JAVA = java
JAVAC = javac -classpath ..:$(srcdir)/..:$$CLASSPATH

.java.class:
	$(JAVAC) -d .. $<

# This redundant rule is needed for some versions of make, but not GNU make.
repl.class:  $(srcdir)/repl.java
	$(JAVAC) -d . $<

javadir=$(prefix)/java/$(subdir)

OBJ = Shell.class repl.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES=$(OBJ) kawa$(BAT) tmp

kawa$(BAT):
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(prefix)/java}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
	chmod +x tmp
	mv tmp kawa$(BAT)

all: $(OBJ) kawa

# Forve repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  repl.class
