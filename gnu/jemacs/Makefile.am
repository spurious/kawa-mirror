## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = lang $(SWING_SUBDIRS)
if WITH_SWING
SWING_SUBDIRS = buffer lisp testsuite
endif
EXTRA_DIST = jar-manifest status.html
