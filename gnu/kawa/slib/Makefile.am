## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/Make-rules

PACKAGE_CFNAME = gnu-kawa-slib
ENABLE_GCJ_COMPILED = $(OFILES_DIR)/kawa1$(exeext) \
  $(OFILES_DIR)/$(PACKAGE_CFNAME).o

java_SCM = genwrite.scm pp.scm ppfile.scm printf.scm readtable.scm \
  $(SCMSOURCES_@XML_SELECTED@)
SOURCES = $(java_SCM)
SCMSOURCES_XML_ENABLED = XML.scm
EXTRA_DIST = $(java_SCM) XML.scm

all: Makefile scm-classes.stamp $(@GCJ_COMPILED_SELECTED@_CDEP) $(@GCJ_COMPILED_SELECTED@)

scm-classes.stamp:  ../../../kawa/classjava.stamp

../../../kawa/classjava.stamp:
	cd ../../../kawa; $(MAKE)

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
