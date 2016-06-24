## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

java_ELISP = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el
java_SCM = MiscOps.scm NumberOps.scm emacs.scm

PACKAGE_FNAME = gnu-jemacs-lisp

EXTRA_DIST = $(java_ELISP) $(java_SCM)

all: Makefile scm-classes.stamp elisp-classes.stamp
