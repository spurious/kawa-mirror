## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_JAVA=\
  CLispReader.java \
  CommonLisp.java

PACKAGE_FNAME = gnu-commonlisp-lang

all: $(@GCJ_COMPILED_SELECTED@)
