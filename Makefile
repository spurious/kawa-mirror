 
.SUFFIXES : .java .class
 
#=========================================================================
# Unix version
#=========================================================================
JAVAC=javac -classpath ..:$$CLASSPATH
CLASSES=../classes
CLASSESDIR=$(CLASSES)/
 
$(CLASSESDIR)%.class: %.java
	$(JAVAC) -d $(CLASSES) $<
 
#=========================================================================
# WIN32 version (NMAKE)
#=========================================================================
#JAVAC=javac -classpath ..;%CLASSPATH%
#CLASSES=..\classes
#CLASSESDIR=..\classes\
#.java{\}.class:
#       $(JAVAC) -d $(CLASSES) $(*F).java
 

OBJ=\
$(CLASSESDIR)kawa.class

all: subdirs $(OBJ)

subdirs:
	cd kawa; make

$(CLASSESDIR)kawa.class: kawa.java
