## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Button.java \
  DrawImage.java \
  DrawShape.java \
  FillShape.java \
  MenuItem.java \
  Paintable.java \
  ViewContainer.java \
  Viewable.java \
  WithComposite.java \
  WithPaint.java \
  WithTransform.java

PACKAGE_FNAME = gnu-kawa-models
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
