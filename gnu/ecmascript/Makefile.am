## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  Reserved.java BinaryOp.java Convert.java \
  Lexer.java Parser.java Prompter.java

PACKAGE_FNAME = gnu-ecmascript
GCJ_INPUTS =  $(java_Java)

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
