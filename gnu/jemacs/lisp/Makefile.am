## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

java_ELISP = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el

GCJ_DEPS = elisp-classes.stamp
PACKAGE_CFNAME = gnu-jemacs-lisp
CLEANFILES = *.class *.stamp $(OFILES_DIR)/$(PACKAGE_FNAME).o

SOURCES = $(java_ELISP)

CLEANFILES = *.class *.stamp

all: Makefile $(@SWING_SELECTED@)

WITH_SWING = $(@GCJ_COMPILED_SELECTED@_CDEP) elisp-classes.stamp $(@GCJ_COMPILED_SELECTED@)
