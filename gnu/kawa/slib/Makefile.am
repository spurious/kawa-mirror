## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

@Make_rules@

PACKAGE_FNAME = gnu-kawa-slib
EXTRA_GCJ_INPUTS =  *.class
ENABLE_GCJ_COMPILED = $(OFILES_DIR)/kawa1$(exeext) \
  $(OFILES_DIR)/$(PACKAGE_FNAME).o

java_SCM = genwrite.scm pp.scm ppfile.scm printf.scm readtable.scm \
  $(SCMSOURCES_@XML_SELECTED@)
SOURCES = $(java_SCM)
SCMSOURCES_XML_ENABLED = XML.scm
EXTRA_DIST = $(java_SCM) XML.scm
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

all: Makefile  $(@GCJ_COMPILED_SELECTED@_CDEP) ../../../kawa/java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
