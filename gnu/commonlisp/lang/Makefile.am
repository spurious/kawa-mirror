## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources = \
  CLispReader.java \
  Lisp2.java \
  defun.java setq.java defvar.java \
  Symbol.java \
  SymbolTable.java \
  UnwindProtect.java \
  CommonLisp.java

PACKAGE_FNAME = gnu-commonlisp-lang

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
