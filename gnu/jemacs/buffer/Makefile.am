## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_WITH_SWING = \
  Buffer.java \
  BufferContent.java \
  BufferKeymap.java \
  BufferReader.java \
  BufferWriter.java \
  CancelledException.java \
  FinalAction.java \
  Frame.java \
  InsertAction.java \
  Marker.java \
  MarkerReader.java \
  Menu.java \
  Modeline.java \
  PrefixAction.java \
  ProcAction.java \
  ReplBuffer.java \
  TooLongAction.java \
  Window.java

EXTRA_JAVA = $(java_WITH_SWING)

java_JAVA=\
  $(java_@SWING_SELECTED@) \
  Signal.java

EXTRA_DIST = emacs.scm overview.html

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.buffer. -C

SCMSOURCES = emacs.scm

CLEANFILES = *.class *.stamp

scmclasses.stamp: $(SCMSOURCES)
	cd ../../../kawa; $(MAKE) 
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) $(KAWAFLAGS) $?
	echo timestamp > scmclasses.stamp

WITH_SWING: classjava.stamp scmclasses.stamp

WITHOUT_SWING:

all: Makefile @SWING_SELECTED@

