## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

#java_CLISP = lists.lisp
java_SCM = PrimOps.scm

GCJ_DEPS = clisp-classes.stamp
PACKAGE_FNAME = gnu-commonlisp-lisp
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

EXTRA_DIST = $(java_SCM)

CLEANFILES = *.class *.stamp
EXTRA_GCJ_INPUTS = *.class
all: Makefile ../../../kawa/java-classes.stamp $(@GCJ_COMPILED_SELECTED@_CDEP) scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)
