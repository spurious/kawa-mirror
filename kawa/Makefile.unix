 
.SUFFIXES : .java .class

#=========================================================================
# Unix version 
#=========================================================================
JAVAC=javac -classpath ..:$$CLASSPATH
CLASSES=../../classes
CLASSESDIR=$(CLASSES)/kawa/

$(CLASSESDIR)%.class: %.java
	$(JAVAC) -d $(CLASSES) $<

#=========================================================================
# WIN32 version (NMAKE)
#=========================================================================
#JAVAC=javac -classpath ..;%CLASSPATH%
#CLASSES=..\..\classes
#CLASSESDIR=..\..\classes\kawa\
#.java{\}.class:
#	$(JAVAC) -d $(CLASSES) $(*F).java

EXCEPTIONS=

OBJ=\
$(CLASSESDIR)Shell.class \
$(EXCEPTIONS)

all: subdirs $(OBJ)

subdirs:
	cd lang; make
	cd standard; make

# Exceptions

# Code
$(CLASSESDIR)Shell.class: Shell.java
