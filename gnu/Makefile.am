## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUBDIRS = bytecode math lists text mapping expr \
	kawa commonlisp ecmascript $(XML_SUBDIRS) $(SWING_SUBDIRS)
if ENABLE_XML
XML_SUBDIRS = xml xquery
endif
if WITH_SWING
SWING_SUBDIRS = jemacs
endif
EXTRA_DIST = PACKAGES

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
