package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.ClassType;
import gnu.bytecode.Type;
import gnu.expr.*;
import gnu.lists.*;

public class prim_method extends Syntax
{
  public static final prim_method virtual_method = new prim_method(182);
  static { virtual_method.setName("primitive-virtual-method"); }

  public static final prim_method static_method = new prim_method(184);
  static { static_method.setName("primitive-static-method"); }

  public static final prim_method interface_method = new prim_method(185);
  static { interface_method.setName("primitive-interface-method"); }

  public static final prim_method constructor = new prim_method(183);
  static { constructor.setName("primitive-constructor"); }

  public static final prim_method op1 = new prim_method();
  static { op1.setName("primitive-op1"); }

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

  public Expression rewrite (Object obj, Translator tr)
  {
    Object[] match = new Object [4];
    if (! (op_code == 0 ? pattern3.match(obj, match, 1)
	   : op_code == 183 ? pattern2.match(obj, match, 2) // constructor
	   : pattern4.match(obj, match, 0))) // virtual or static
      return tr.syntaxError ("wrong number of arguments to "+getName()
			     +"(opcode:"+op_code+")");

    if (! (match[3] instanceof LList))
      return tr.syntaxError ("missing/invalid parameter list in "+getName());
    LList argp = (LList) match[3];

    int narg = argp.size();
    Type[] args = new Type[narg];
    for (int i = 0;  i < narg;  i++)
      {
	Pair p = (Pair)argp;
	args[i] = tr.exp2Type(p);
	argp = (LList)p.cdr;
      }
    Type rtype = tr.exp2Type(new Pair(match[2], null));
    PrimProcedure proc;
    if (op_code == 0)
      {
	int opcode = ((Number)(match[1])).intValue();
	proc = new PrimProcedure(opcode, rtype, args);
      }
    else
      {
        ClassType cl = null;
        Type ctype;
        int carg;
        if (op_code == 183)
          {
            carg = 2;
            ctype = rtype;
          }
        else
          {
            carg = 0;
            ctype = tr.exp2Type((Pair) obj);
          }
        try
          {
            cl = (ClassType) ctype;
            cl.getReflectClass();
          }
        catch (Exception ex)
          {
            char code;
            if (cl == null)
              code = 'e';
            else
              {
                code = 'w';
                ((ClassType) cl).setExisting(false);
              }
            tr.error(code, "unknown class: " + match[carg]);
          }
        if (op_code == 183)  // primitive-constructor
          {
            proc = new PrimProcedure(cl, args);
          }
        else
          {
            Pair p;
            if (match[1] instanceof Pair
                && (p = (Pair) match[1]).car == "quote")
              match[1] = ((Pair) p.cdr).car;
            proc = new PrimProcedure(op_code, cl,
                                     match[1].toString(), rtype, args);
          }
      }
    return new QuoteExp(proc);
  }
}
