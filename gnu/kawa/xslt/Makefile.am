## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  TemplateTable.java \
  XSLT.java \
  XslTranslator.java

PACKAGE_FNAME = gnu-kawaxslt

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
