## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class .el

javadir=$(datadir)/java/$(subdir)

CLASSPATH_ENV = CLASSPATH=$(JAVAROOT):$(srcdir)/$(JAVAROOT):$$CLASSPATH
JAVAROOT = $(top_builddir)

java_WITH_SWING =

EXTRA_JAVA = $(java_WITH_SWING)

java_JAVA=\
  $(java_@SWING_SELECTED@)

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.lisp. -C

ELISPSOURCES = alist.el hanoi.el rect.el autoloads.el

CLEANFILES = *.class *.stamp

WITH_SWING: elclasses.stamp

WITHOUT_SWING:

all: Makefile @SWING_SELECTED@

elclasses.stamp: $(ELISPSOURCES)
	cd ../../../kawa; $(MAKE)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) --elisp $(KAWAFLAGS) $?
	echo timestamp > elclasses.stamp
