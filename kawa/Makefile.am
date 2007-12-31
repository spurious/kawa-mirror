## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

SUBDIRS = lang standard lib

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

if WITH_AWT
java_WITH_AWT = \
  GuiConsole.java ReplPane.java ReplDocument.java \
  ReplPaneOutPort.java GuiInPort.java
endif

java_sources = Shell.java repl.java Version.java \
  TelnetInputStream.java TelnetOutputStream.java Telnet.java TelnetRepl.java \
  $(java_WITH_AWT)

EXTRA_DIST = Version.java.in $(java_WITH_AWT)

PACKAGE_FNAME = kawa-repl
GCJ_INPUTS =  $(java_Java)

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
