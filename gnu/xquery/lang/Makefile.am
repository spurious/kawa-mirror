## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  XQuery.java \
  XQParser.java

PACKAGE_FNAME = gnu-xquery-lang

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
