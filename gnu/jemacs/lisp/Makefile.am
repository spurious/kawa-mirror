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

SOURCES = $(ELISPSOURCES)

CLEANFILES = *.class *.stamp

all: Makefile el_@SWING_SELECTED@.stamp

el_WITH_SWING.stamp: $(ELISPSOURCES)
	cd ../../../kawa; $(MAKE)
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) --elisp $(KAWAFLAGS) $?
	echo timestamp > el_WITH_SWING.stamp

el_WITHOUT_SWING.stamp:
	echo timestamp > el_WITHOUT_SWING.stamp
