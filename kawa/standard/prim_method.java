package kawa.standard;
import kawa.lang.*;
import codegen.Access;
import codegen.ClassType;
import codegen.Type;

// (primitive-static-method "class" "method" "rettype" ("argtype" ...))
// (primitive-virtual-method "class" "method" "rettype" ("argtype" ...))
// (primitive-op1 opcode "rettype"  ("argtype" ...))

class prim_method extends Syntax
{
  static private Pattern pattern3 = new ListPat (3);
  static private Pattern pattern4 = new ListPat (4);

  // 0: primitive-op1
  // 1: primitive-virtual-method
  // 2: primitive-static-method
  int kind;

  public prim_method (boolean isStatic)
  {
    kind = isStatic ? 2 : 1;
  }

  public prim_method ()
  {
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Object[] match = new Object [4];
    if (! (kind == 0 ? pattern3.match(obj, match, 1)
	   : pattern4.match(obj, match, 0)))
      return tr.syntaxError ("wrong number of arguments to "+name());

    List argp = (List) match[3];

    int narg = argp.length();
    Type[] args = new Type[narg];
    for (int i = 0;  i < narg;  i++)
      {
	Pair p = (Pair)argp;
	args[i] = PrimProcedure.string2Type(p.car.toString());
	argp = (List)p.cdr;
      }
    Type rtype = PrimProcedure.string2Type(match[2].toString());
    PrimProcedure proc;
    if (kind == 0)
      {
	int opcode = ((Number)(match[1])).intValue();
	proc = new PrimProcedure(opcode, rtype, args);
      }
    else
      {
	ClassType cl = (ClassType)
	  PrimProcedure.string2Type (match[0].toString());
	proc = new PrimProcedure (cl, match[1].toString(), rtype, args,
				  kind == 2 ? Access.STATIC : 0);
      }
    return new QuoteExp(proc);
  }
}
