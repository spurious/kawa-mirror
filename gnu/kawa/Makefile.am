## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = util reflect functions lispexpr $(XML_SUBDIR) \
  $(SAX_SUBDIR) $(SERVLET_SUBDIRS) ant models $(SWING_SUBDIRS) slib
if ENABLE_SERVLET
SERVLET_SUBDIRS = servlet
endif
if ENABLE_XML
XML_SUBDIR = xml brl xslt
if WITH_SAX2
SAX_SUBDIR = sax
endif
endif
if WITH_SWING
SWING_SUBDIRS = swingviews
endif

