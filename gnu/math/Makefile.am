## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

CLASSPATH = ../..:$(srcdir)/../..:$$CLASSPATH
# Use an environment variable for CLASSPATH so we pick up standard classes:
CLASSPATH_ENV = CLASSPATH=$(CLASSPATH)
JAVAC = javac
JCFLAGS = -g

.java.class:
	$(CLASSPATH_ENV) $(JAVAC) $(JCFLAGS) -d ../.. $<

javadir=$(datadir)/java/$(subdir)


OBJ=\
  Numeric.class \
  Quantity.class DQuantity.class CQuantity.class \
  Complex.class CComplex.class DComplex.class \
  BaseUnit.class Unit.class MulUnit.class Dimensions.class \
  RealNum.class DFloNum.class \
  RatNum.class \
  IntFraction.class \
  MPN.class \
  IntNum.class \
  BitOps.class

java_DATA=$(OBJ)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.java)

CLEANFILES = *.class

all: $(OBJ)

HTML_HOME = $$HOME/public_html

install-html:
	-mkdir $(HTML_HOME)/gnu.math
	cp $(srcdir)/overview.html $(HTML_HOME)/gnu.math/index.html
	cp $(srcdir)/COPYING $(HTML_HOME)/gnu.math/COPYING
	chmod a+r $(HTML_HOME)/gnu.bytecode/*
