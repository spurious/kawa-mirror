## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  BufferContent.java \
  BufferReader.java \
  MarkerReader.java \
  SwingBuffer.java \
  SwingFrame.java \
  SwingToolkit.java \
  SwingWindow.java

PACKAGE_FNAME = gnu-jemacs-swing

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@_CDEP)
