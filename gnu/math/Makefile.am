## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  Numeric.java \
  Quantity.java DQuantity.java CQuantity.java \
  Complex.java CComplex.java DComplex.java \
  BaseUnit.java Unit.java MulUnit.java Dimensions.java \
  RealNum.java DFloNum.java \
  RatNum.java \
  IntFraction.java \
  MPN.java \
  IntNum.java \
  BitOps.java

HTML_HOME = $$HOME/public_html

install-html:
	-mkdir $(HTML_HOME)/gnu.math
	cp $(srcdir)/overview.html $(HTML_HOME)/gnu.math/index.html
	cp $(srcdir)/COPYING $(HTML_HOME)/gnu.math/COPYING
	chmod a+r $(HTML_HOME)/gnu.bytecode/*
