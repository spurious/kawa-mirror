## Process this with automake to create Makefile.in

include $(top_srcdir)/Make-rules

PACKAGE_FNAME = gnu-kawa-slib
EXTRA_GCJ_INPUTS =  *.class

if ENABLE_XML
XML_SCM = XML.scm XStrings.scm

if WITH_SWING
SWING_SCM = swing.scm
endif
endif

# NOTE if this list is changed, also update build.xml.
java_SCM = DefineRecordType.scm enums.scm readtable.scm \
  srfi1.scm srfi2.scm \
  conditions.scm srfi13.scm srfi34.scm \
  srfi37.scm srfi69.scm pregexp.scm \
  genwrite.scm pp.scm ppfile.scm printf.scm syntaxutils.scm \
  cut.scm testing.scm $(XML_SCM) gui.scm $(SWING_SCM)
EXTRA_DIST = $(java_SCM) XML.scm swing.scm

all: Makefile scm-classes.stamp $(@GCJ_COMPILED_SELECTED@)

