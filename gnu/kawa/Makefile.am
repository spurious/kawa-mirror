## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = util reflect functions lispexpr $(XML_SUBDIR) \
  slib $(SERVLET_SUBDIRS) ant
if ENABLE_SERVLET
SERVLET_SUBDIRS = servlet
endif
if ENABLE_XML
XML_SUBDIR = xml brl
endif

