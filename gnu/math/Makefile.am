## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

JAVAC=javac -classpath ../..:$(srcdir)/../..:$$CLASSPATH

.java.class:
	$(JAVAC) -d ../.. $<

javadir=$(prefix)/java/$(subdir)


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
