## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class .el

javadir=$(datadir)/java/$(subdir)

JAVA = @JAVA@
CLASSPATH_ENV = CLASSPATH=$(JAVAROOT):$(srcdir)/$(JAVAROOT):$$CLASSPATH
JAVAROOT = $(top_builddir)

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.jemacs.lisp. -C

ELISPSOURCES = primitives.el alist.el simple.el keymap.el keydefs.el hanoi.el \
  rect.el editfns.el subr.el autoloads.el

CLEANFILES = *.class *.stamp

all: Makefile elclasses.stamp

elclasses.WITH_SWING: $(ELISPSOURCES)
	cd ../../../kawa; $(MAKE)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) --elisp $(KAWAFLAGS) $?
	echo timestamp > elclasses.stamp

elclasses.WITHOUT_SWING:
	echo timestamp > elclasses.stamp

elclasses.stamp: elclasses.@SWING_SELECTED@
