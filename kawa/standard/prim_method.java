package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.Access;
import gnu.bytecode.ClassType;
import gnu.bytecode.Type;

// OPC: (primitive-op1 OPC "rettype"  ("argtype" ...))
// 182: (primitive-virtual-method "class" "method" "rettype" ("argtype" ...))
// 183: (primitive-constructor "class" ("argtype ...))
// 184: (primitive-static-method "class" "method" "rettype" ("argtype" ...))
// 185: (primitive-interface-method "class" "method" "rettype" ("argtype" ...))

class prim_method extends Syntax
{
  static private Pattern pattern2 = new ListPat (2);
  static private Pattern pattern3 = new ListPat (3);
  static private Pattern pattern4 = new ListPat (4);

  int op_code;

  int opcode () { return op_code; }

  public prim_method (int opcode)
  {
    op_code = opcode;
  }

  public prim_method ()
  {
  }

  static Type exp2Type (Object obj, Translator tr)
  {
    String str = obj.toString();
    if (obj instanceof kawa.lang.FString)
      return PrimProcedure.string2Type(str);
    else if (obj instanceof String)
      {
	int len = str.length();
	if (len > 2
	    && str.charAt(0) == '<'
	    && str.charAt(len-1) == '>')
	  {
	    String tstr = str.substring(1, len-1);
	    if (Type.isValidJavaTypeName(tstr))
	      return PrimProcedure.string2Type(tstr);
	  }
      }
    tr.syntaxError("invalid type spec (must be \"type\" or <type>: " + str);
    return Type.pointer_type;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Object[] match = new Object [4];
    if (! (op_code == 0 ? pattern3.match(obj, match, 1)
	   : op_code == 183 ? pattern2.match(obj, match, 2) // constructor
	   : pattern4.match(obj, match, 0))) // virtual or static
      return tr.syntaxError ("wrong number of arguments to "+name()
			     +"(opcode:"+op_code+")");

    if (! (match[3] instanceof List))
      return tr.syntaxError ("missing/invalid parameter list in "+name());
    List argp = (List) match[3];

    int narg = argp.length();
    Type[] args = new Type[narg];
    for (int i = 0;  i < narg;  i++)
      {
	Pair p = (Pair)argp;
	args[i] = exp2Type(p.car, tr);
	argp = (List)p.cdr;
      }
    Type rtype = exp2Type(match[2], tr);
    PrimProcedure proc;
    if (op_code == 0)
      {
	int opcode = ((Number)(match[1])).intValue();
	proc = new PrimProcedure(opcode, rtype, args);
      }
    else if (op_code == 183)  // primitive-constructor
      {
	proc = new PrimProcedure((ClassType) rtype, args);
      }
    else
      {
	ClassType cl = (ClassType) exp2Type(match[0], tr);
	proc = new PrimProcedure(op_code, cl, match[1].toString(), rtype,args);
      }
    return new QuoteExp(proc);
  }
}
