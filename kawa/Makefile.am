## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang standard lib

SUFFIXES = .java .class

bin_SCRIPTS = kawa

# Suffix for batch files.
BAT=

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

javadir=$(datadir)/java/$(subdir)

java_JAVA = Shell.java repl.java Version.java \
  GuiConsole.java MessageArea.java TextAreaWriter.java GuiInPort.java

CLEANFILES = *.class kawa$(BAT) tmp *.stamp
BUILT_SOURCES = Version.java
SOURCES = $(java_JAVA) Version.java.in

Version.java: $(srcdir)/Version.java.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/Version.java.in >Version.java

kawa$(BAT):
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
	chmod +x tmp
	mv tmp kawa$(BAT)

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  classjava.stamp kawa

EXTRA_DIST = Version.java
