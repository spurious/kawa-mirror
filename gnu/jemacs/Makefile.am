## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = lang $(GUI_SUBDIRS)
if WITH_SWING
if WITH_SWT
GUI_SUBDIRS = buffer lisp testsuite swing swt
else
GUI_SUBDIRS = buffer lisp testsuite swing
endif
else
GUI_SUBDIRS = buffer lisp testsuite swt
endif
EXTRA_DIST = jar-manifest status.html
