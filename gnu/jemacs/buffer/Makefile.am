## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_WITH_SWING = \
  Buffer.java \
  BufferContent.java \
  BufferKeymap.java \
  BufferLocalConstraint.java \
  BufferReader.java \
  BufferWriter.java \
  CancelledException.java \
  Command.java \
  FinalAction.java \
  Frame.java \
  InfProcessMode.java \
  InputStreamHandler.java \
  InsertAction.java \
  Marker.java \
  MarkerReader.java \
  Menu.java \
  Mode.java \
  Modeline.java \
  ProcessMode.java \
  ReplBuffer.java \
  TelnetMode.java \
  TooLongAction.java \
  Window.java

EXTRA_JAVA = $(java_WITH_SWING)

java_JAVA=\
  $(java_@SWING_SELECTED@) \
  Signal.java

EXTRA_DIST = emacs.scm package.html

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

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
