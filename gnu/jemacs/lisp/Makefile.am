## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

java_ELISP = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el

GCJ_DEPS = elisp-classes.stamp
PACKAGE_FNAME = gnu-jemacs-lisp
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

EXTRA_DIST = $(java_ELISP)

CLEANFILES = *.class *.stamp

all: Makefile $(@SWING_SELECTED@)

WITH_SWING = $(@GCJ_COMPILED_SELECTED@_CDEP) elisp-classes.stamp $(@GCJ_COMPILED_SELECTED@)
