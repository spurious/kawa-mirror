## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  ApplyTemplates.java \
  TemplateTable.java \
  XSLT.java \
  XslTranslator.java

PACKAGE_FNAME = gnu-kawa-xslt

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
