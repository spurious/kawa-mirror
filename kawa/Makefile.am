## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = lang standard lib

SUFFIXES = .java .class

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
SOURCES = $(java_JAVA) Version.java.in kawa.c

kawa.sh:
	echo '#!/bin/sh' >tmp
	echo 'KAWALIB=$${KAWALIB-$(datadir)/java}' >>tmp
	echo 'CLASSPATH=$${KAWALIB}:$${CLASSPATH-$(CLASSPATH)}' >>tmp
	echo 'export CLASSPATH' >>tmp
	if JAVA=`which $(JAVA)`; then true; else JAVA=$(JAVA); fi; \
	  echo 'exec $${JAVA-'$${JAVA}'} kawa.repl "$$@"' >>tmp
	chmod +x tmp
	mv tmp kawa.sh

READLINE_INCLUDE_PATH =
READLINE_LIB_PATH =
kawa_EXTRA_LIBS = -lncurses  # For Linux
#kawa_EXTRA_LIBS = -lcurses -lsocket # For Solaris2
DEFS = -DKAWALIB='"$(datadir)/java"' $(READLINE_INCLUDE_PATH)
kawa_LDADD = $(READLINE_LIB_PATH) -lreadline -lhistory $(kawa_EXTRA_LIBS)

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  classjava.stamp

EXTRA_DIST = Version.java.in Version.java

install-exec-local: install-exec-bindir

install-exec-bindir: $(noninst_SCRIPTS)
	$(mkinstalldirs) $(bindir)
	if test "@kawa_bin@" = "" ; then \
	  $(INSTALL_SCRIPT) kawa.sh $(DESTDIR)$(bindir)/kawa; \
	fi
