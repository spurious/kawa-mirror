## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_SWING = \
  Buffer.java \
  BufferContent.java \
  BufferKeymap.java \
  BufferWriter.java \
  CancelledException.java \
  FinalAction.java \
  Frame.java \
  InsertAction.java \
  Marker.java \
  Modeline.java \
  PrefixAction.java \
  ProcAction.java \
  ReplBuffer.java \
  Signal.java \
  TooLongAction.java \
  Window.java
  
EXTRA_JAVA = $(java_SWING)

java_JAVA=\
  @extra_java@ \
  Emacs.java

KAWA = $(JAVA) kawa.repl

all: Makefile classjava.stamp

