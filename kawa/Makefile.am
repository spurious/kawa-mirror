## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

SUBDIRS = lang standard lib

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

java_WITH_AWT = \
  GuiConsole.java MessageArea.java TextAreaWriter.java GuiInPort.java

java_sources = Shell.java repl.java Version.java \
  TelnetInputStream.java TelnetOutputStream.java Telnet.java\
  $(java_@AWT_SELECTED@)

EXTRA_DIST = Version.java.in $(java_WITH_AWT)

PACKAGE_FNAME = kawa-repl
GCJ_INPUTS =  $(java_Java)

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)

