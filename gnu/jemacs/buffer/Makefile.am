## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_SWING = \
  Buffer.java \
  Frame.java \
  Keymap.java \
  Marker.java \
  Modeline.java \
  ProcCommand.java \
  Signal.java \
  Window.java
  
EXTRA_JAVA = $(java_SWING)

java_JAVA=\
  @extra_java@ \
  Emacs.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

