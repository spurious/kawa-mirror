// This is temporary wrapper to add a main() for dump.main,
// when compield by gcc, and linked with libjava.
// It will be replaced by something generated automatically.

#include "java-lang.h"

struct gnu
{
  struct bytecode;
};

struct gnu::bytecode
{
  class dump;
};

class gnu::bytecode::dump {
public:
  static main(JArray<jstring>*);
};

extern java::lang::Class dump_class asm("_Q33gnu8bytecode4dump.class");

extern "C" void soft_initialise_class(jclass);
int
main (int argc, const char** argv)
{
  // initialize runtime?
  // initialize dump class.
  soft_initialise_class (&dump_class);
  gnu::bytecode::dump::main(JvConvertArgv(argc-1, argv+1));
}
