## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .class

javadir=$(datadir)/java/$(subdir)

java_JAVA=\
  Char.java \
  LineBufferedReader.java SourceError.java Lexer.java SyntaxException.java \
  QueueReader.java SourceMessages.java NullReader.java \
  ReportFormat.java LiteralFormat.java PadFormat.java CompoundFormat.java \
  IntegerFormat.java RomanIntegerFormat.java EnglishIntegerFormat.java \
  CaseConvertFormat.java FlushFormat.java

EXTRA_DIST = package.html
