## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  SwingButton.java \
  SwingFrame.java \
  SwingContainer.java \
  SwingContent.java \
  SwingPaintable.java

PACKAGE_FNAME = gnu-kawa-swingviews
GCJ_INPUTS =  $(java_Java)

if WITH_SWING
ALL = java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)
