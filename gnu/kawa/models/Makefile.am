## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Button.java \
  MenuItem.java \
  ViewContainer.java \
  Viewable.java

PACKAGE_FNAME = gnu-kawa-models
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
