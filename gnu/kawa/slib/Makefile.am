## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

PACKAGE_FNAME = gnu-kawa-slib
EXTRA_GCJ_INPUTS =  *.class
EXTRA_GCJ_COMPILED = $(OFILES_DIR)/kawa1$(exeext)

if ENABLE_XML
XML_SCM = XML.scm XStrings.scm
endif
if ENABLE_SERVLET
SERVLET_SCM = HTTP.scm
endif
# NOTE if this list is changed, also update build,xml.
java_SCM = genwrite.scm pp.scm ppfile.scm printf.scm srfi1.scm readtable.scm \
  $(XML_SCM) $(SERVLET_SCM)
EXTRA_DIST = $(java_SCM) XML.scm HTTP.scm
KAWA1 = $(@GCJ_COMPILED_SELECTED@_KAWA1)

all: Makefile  $(@GCJ_COMPILED_SELECTED@_CDEP) ../../../kawa/java-classes.stamp scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

../../../kawa/java-classes.stamp:
	cd ../../../kawa; $(MAKE)

# Used to generate makekawa.bat in ../../.. .
list-sources:
	echo $(SCMSOURCES)
