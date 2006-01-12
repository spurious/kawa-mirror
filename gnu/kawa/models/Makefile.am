## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Box.java \
  Button.java \
  Column.java \
  Display.java \
  DrawImage.java \
  DrawShape.java \
  FillShape.java \
  Label.java \
  Model.java \
  ModelListener.java \
  MenuItem.java \
  Paintable.java \
  Row.java \
  Spacer.java \
  Text.java \
  Viewable.java \
  WeakListener.java \
  Window.java \
  WithComposite.java \
  WithPaint.java \
  WithTransform.java

PACKAGE_FNAME = gnu-kawa-models
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
