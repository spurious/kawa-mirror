## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  Numeric.java \
  Quantity.java DQuantity.java CQuantity.java \
  Complex.java CComplex.java DComplex.java \
  NamedUnit.java BaseUnit.java Unit.java MulUnit.java Dimensions.java \
  RealNum.java DFloNum.java \
  RatNum.java \
  IntFraction.java \
  MPN.java \
  IntNum.java \
  BitOps.java \
  FixedRealFormat.java \
  ExponentialFormat.java

EXTRA_DIST = package.html
