## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

.SUFFIXES : .java .class

JAVAC=javac -classpath ../..:$$CLASSPATH -d @TOP_BUILDDIR@

%.class: %.java
	(cd $(srcdir); $(JAVAC) $<)

javadir=$(prefix)/java/kawa/lang

EXCEPTIONS=\
EOFInComment.class \
UnexpectedCloseParen.class \
InvalidPoundConstruct.class \
EOFInString.class \
NumberTooLong.class \
InvalidCharacterName.class \
MalformedList.class \
UnboundSymbol.class \
WrongArguments.class \
WrongType.class \
GenericError.class \
NotImplemented.class

OBJ=\
Printable.class \
Named.class \
snull.class \
pair.class \
print.class \
iport.class \
oport.class \
symbol.class \
vector.class \
Executable.class \
Syntaxable.class \
Exit.class \
Undefined.class \
Lambda.class \
LambdaProcedure.class \
Quote.class \
QuasiQuote.class \
Unquote.class \
UnquoteSplicing.class \
Interpreter.class \
Procedure0.class \
Procedure1.class \
Procedure2.class \
Procedure3.class \
Procedure4.class \
Syntax0.class \
Syntax1.class \
Syntax2.class \
Syntax3.class \
Syntax4.class \
$(EXCEPTIONS)

java_DATA=$(OBJ)

all: $(OBJ)

