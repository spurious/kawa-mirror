## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

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

java_SCM = emacs.scm

EXTRA_DIST = emacs.scm $(EXTRA_JAVA) package.html

GCJ_INPUTS = $(java_WITH_SWING) *emacs*.class
GCJ_DEPS = $(java_WITH_SWING) scm-classes.stamp
PACKAGE_CFNAME = gnu-jemacs-buffer
CLEANFILES = *.class *.stamp $(OFILES_DIR)/$(PACKAGE_CFNAME).o

WITH_SWING: classjava.stamp $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

WITHOUT_SWING:

all: Makefile @SWING_SELECTED@

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
