## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  SwingButton.java \
  SwingFrame.java \
  SwingContainer.java

PACKAGE_FNAME = gnu-kawa-swingviews
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
