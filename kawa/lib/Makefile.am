## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = foreign

SUFFIXES = .java .scm .class

JAVA = java
CLASSPATH = ../..:$(srcdir)/../..:$$CLASSPATH
# Use an environment variable for CLASSPATH so we pick up standard classes:
CLASSPATH_ENV = CLASSPATH=$(CLASSPATH)

JAVAC = javac
JCFLAGS = -g
KAWA = $(JAVA) kawa.repl

.java.class:
	$(CLASSPATH_ENV) $(JAVAC) $(JFLAGS) -d ../.. $<

.scm.class:
	$(CLASSPATH_ENV) $(KAWA) -d ../.. -P $(subdir). -C $< 

misc.class: $(srcdir)/misc.scm
	$(CLASSPATH_ENV) $(KAWA) \
	  -e '(define-syntax %V (syntax-rules () ((%V) "@VERSION@")))' \
	  -d ../.. -P $(subdir). -C $(srcdir)/misc.scm

javadir=$(prefix)/java/$(subdir)

OBJ=\
  std_syntax.class \
  thread.class \
  characters.class \
  files.class \
  keywords.class \
  ports.class \
  misc.class \
  quantities.class \
  strings.class \
  syntax.class \
  vectors.class \
  reflection.class

# These are generated automatically when the $(OBJ) files are built,
# but they also need to be installed.
# The funny names are because the Scheme functions names get "mangled"
# into valid Java class names.
SECONDARY_OBJS=\
  characters$$char_2_integer.class \
  characters$$char__downcase.class \
  characters$$char__lower__case_P.class \
  characters$$char__numeric_P.class \
  characters$$char__upcase.class \
  characters$$char__upper__case_P.class \
  characters$$char__whitespace_P.class \
  characters$$integer_2_char.class \
  files$$file__exists_P.class \
  files$$file__readable_P.class \
  files$$create__directory.class \
  files$$delete__file.class \
  files$$file__directory_P.class \
  files$$file__writable_P.class \
  files$$rename__file.class \
  files$$_2_pathname.class \
  keywords$$keyword_2_string.class \
  keywords$$keyword_P.class \
  keywords$$string_2_keyword.class \
  misc$$environment__bound_P.class \
  misc$$scheme__implementation__version.class \
  ports$$copy__file.class \
  ports$$default__prompter.class \
  ports$$eof__object_P.class \
  ports$$force__output.class \
  ports$$input__port__column__number.class \
  ports$$input__port__line__number.class \
  ports$$input__port__prompter.class \
  ports$$input__port__read__state.class \
  ports$$newline.class \
  ports$$set__input__port__line__number_B.class \
  ports$$set__input__port__prompter_B.class \
  ports$$transcript__off.class \
  ports$$transcript__on.class \
  strings$$make__string.class \
  strings$$string__length.class \
  syntax$$_Cdefmacro.class \
  syntax$$gentemp.class \
  syntax$$dynamic__wind.class \
  thread$$exit.class \
  vectors$$list_2_vector.class \
  vectors$$vector__length.class \
  vectors$$vector__ref.class \
  vectors$$vector__set_B.class \
  reflection$$lambda.class \
  reflection$$make__record__type.class \
  reflection$$record_P.class \
  reflection$$record__accessor.class \
  reflection$$record__constructor.class \
  reflection$$record__modifier.class \
  reflection$$record__predicate.class \
  reflection$$record__type__descriptor.class \
  reflection$$record__type__field__names.class \
  reflection$$record__type__name.class

java_DATA=$(OBJ) $(SECONDARY_OBJS)

# Following only works for GNU make, but is only needed for make dist or TAGS.
SOURCES = $(OBJ:.class=.scm)

CLEANFILES = *.class

all: $(OBJ)
