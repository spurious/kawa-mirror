## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  BufferContent.java \
  BufferContentReader.java \
  CommandDialog.java \
  LineOffsets.java \
  SwtBuffer.java \
  SwtCharBuffer.java \
  SwtFrame.java \
  SwtMenu.java \
  SwtToolkit.java \
  SwtWindow.java

PACKAGE_FNAME = gnu-jemacs-swt

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@_CDEP)
