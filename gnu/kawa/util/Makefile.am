## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
  ConsumeProc.java

PACKAGE_FNAME = gnu-kawa-util

all: Makefile classjava.stamp $(@GCJ_COMPILED_SELECTED@)
