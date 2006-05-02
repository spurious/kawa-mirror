## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_sources=\
  CatchableException.java \
  While.java \
  ELisp.java \
  AddOp.java \
  DivideOp.java \
  Print.java \
  NumberCompare.java \
  SaveExcursion.java \
  defcustom.java defgroup.java \
  lambda.java

java_SCM = MiscOps.scm NumberOps.scm
PACKAGE_FNAME = gnu-jemacs-lang
EXTRA_GCJ_INPUTS = MiscOps*.class NumberOps*.class

EXTRA_DIST = $(java_SCM) package.html
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
if ENABLE_JEMACS
ALL = java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)

