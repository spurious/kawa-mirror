## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

SUBDIRS = lang standard lib

# Use an environment variable for CLASSPATH so we pick up standard classes:
#CLASSPATH_ENV = CLASSPATH=..:$(srcdir)/..:$$CLASSPATH

javadir=$(datadir)/java/$(subdir)

java_WITH_AWT = \
  GuiConsole.java MessageArea.java TextAreaWriter.java GuiInPort.java

java_JAVA = Shell.java repl.java Version.java \
  TelnetInputStream.java TelnetOutputStream.java Telnet.java\
  $(java_@AWT_SELECTED@)

EXTRA_JAVA = $(java_WITH_AWT)

EXTRA_DIST = Version.java.in

PACKAGE_FNAME = kawa-repl

# Force repl.class to be built before sub-dirs.
# Specifically, lib needs repl to compile scm files.
all-recursive:  classjava.stamp $(@GCJ_COMPILED_SELECTED@)

EXTRA_DIST = Version.java.in Version.java

