## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign
SUBDIRS = bytecode mapping text expr math ecmascript jemacs
EXTRA_DIST = PACKAGES

HTML_HOME = $$HOME/public_html

install-subdir-html:
	-mkdir $(HTML_HOME)/gnu.$(DIR)
	cp $(srcdir)/overview.html $(HTML_HOME)/gnu.$(DIR)/index.html
	cp $(srcdir)/COPYING $(HTML_HOME)/gnu.$(DIR)/COPYING
	chmod a+r $(HTML_HOME)/gnu.$(DIR)/*

install-html:
	for dir in bytecode math mapping; do \
	  (cd $$dir; \
	    $(MAKE) -f ../Makefile install-subdir-html \
	      DIR=$$dir HTML_HOME="$(HTML_HOME)"); \
	done
