## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang standard lib

SUFFIXES = .java .class

bin_SCRIPTS = kawa.sh
bin_PROGRAMS = @kawa_bin@
EXTRA_PROGRAMS = kawa
noinst_SCRIPTS = kawa.sh
kawa_SOURCES = kawa.c

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

javadir=$(datadir)/java/$(subdir)

java_JAVA = Shell.java repl.java Version.java \
  TelnetInputStream.java TelnetOutputStream.java TelnetConnection.java Telnet.java\
  GuiConsole.java MessageArea.java TextAreaWriter.java GuiInPort.java

CLEANFILES = *.class kawa.sh kawa$(EXEEXT) tmp *.stamp
BUILT_SOURCES = Version.java
SOURCES = $(java_JAVA) Version.java.in kawa.c

Version.java: $(srcdir)/Version.java.in $(srcdir)/../configure.in
	sed -e 's|VERSION|@VERSION@|' <$(srcdir)/Version.java.in >Version.java

kawa.sh:
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
	chmod +x tmp
	mv tmp kawa.sh

DEFS = -DKAWALIB='"$(datadir)/java"'
kawa_LDADD = -lreadline -lhistory -lncurses

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  classjava.stamp

EXTRA_DIST = Version.java

install-exec-local: install-exec-bindir

install-exec-bindir: bin_SCRIPTS
	$(mkinstalldirs) $(bindir)
	if test "@kawa_bin@" = "" ; then \
	  $(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa.sh; \
	fi
