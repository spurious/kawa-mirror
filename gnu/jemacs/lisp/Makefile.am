## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_ELISP = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el

PACKAGE_FNAME = gnu-jemacs-lisp
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)
EXTRA_GCJ_COMPILED = ../../../bin/kawa1$(exeext) elisp-classes.stamp
EXTRA_GCJ_INPUTS = *.class

EXTRA_DIST = $(java_ELISP)

if ENABLE_JEMACS
ALL = $(@GCJ_COMPILED_SELECTED@_CDEP) elisp-classes.stamp $(@GCJ_COMPILED_SELECTED@)
endif
all: Makefile $(ALL)
