## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

#java_CLISP = lists.lisp
java_SCM = PrimOps.scm

PACKAGE_FNAME = gnu-commonlisp-lisp

EXTRA_DIST = $(java_SCM)

EXTRA_GCJ_INPUTS = *.class
all: Makefile ../../../kawa/java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)
