## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

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
  IgnoreAction.java \
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

java_sources = \
  $(java_@SWING_SELECTED@) \
  Signal.java

java_SCM = emacs.scm

EXTRA_DIST = emacs.scm $(java_WITH_SWING) package.html
GCJ_INPUTS = $(java_WITH_SWING) *emacs*.class
EXTRA_GCJ_DEPS = scm-classes.stamp
PACKAGE_FNAME = gnu-jemacs-buffer
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

WITH_SWING: java-classes.stamp $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

WITHOUT_SWING:

all: Makefile @SWING_SELECTED@

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
