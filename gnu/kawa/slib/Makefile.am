## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class .scm

javadir=$(datadir)/java/$(subdir)

KAWA = $(JAVA) kawa.repl
KAWAFLAGS = -P gnu.kawa.slib. -C

java_JAVA = SLib.java

SCMSOURCES = genwrite.scm pp.scm ppfile.scm printf.scm \
  $(SCMSOURCES_@XML_SELECTED@)
SCMSOURCES_XML_ENABLED = XML.scm
EXTRA_DIST = $(SCMSOURCES) XML.scm

CLEANFILES = *.class *.stamp

scmclasses.stamp: $(SCMSOURCES)
	cd ../../../kawa; $(MAKE) 
	$(CLASSPATH_ENV) $(KAWA) -d $(JAVAROOT) $(KAWAFLAGS) $?
	echo timestamp > scmclasses.stamp

KAWA = $(JAVA) kawa.repl

all: Makefile scmclasses.stamp

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
