## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = util reflect functions lispexpr $(XML_SUBDIR) \
  $(SAX_SUBDIR) $(SERVLET_SUBDIRS) slib ant
if ENABLE_SERVLET
SERVLET_SUBDIRS = servlet
endif
if ENABLE_XML
XML_SUBDIR = xml brl xslt
if WITH_SAX2
SAX_SUBDIR = sax
endif
endif

