## Process this with automake to create Makefile.in

SUBDIRS = bytecode math lists $(XML_SUBDIR) text mapping expr \
	kawa commonlisp ecmascript $(BRL_SUBDIR) $(XQUERY_SUBDIR) \
	$(SWING_SUBDIRS)
if ENABLE_XML
XML_SUBDIR = xml
BRL_SUBDIR = brl
XQUERY_SUBDIR = xquery
endif
if WITH_SWING
SWING_SUBDIRS = jemacs
endif

HTML_HOME = $$HOME/public_html

install-subdir-html:
	-mkdir $(HTML_HOME)/gnu.$(DIR)
	cp $(srcdir)/package.html $(HTML_HOME)/gnu.$(DIR)/index.html
	cp $(srcdir)/COPYING $(HTML_HOME)/gnu.$(DIR)/COPYING
	chmod a+r $(HTML_HOME)/gnu.$(DIR)/*

install-html:
	for dir in bytecode math mapping; do \
	  (cd $$dir; \
	    $(MAKE) -f ../Makefile install-subdir-html \
	      DIR=$$dir HTML_HOME="$(HTML_HOME)"); \
	done
