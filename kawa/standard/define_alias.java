package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class define_alias extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof String && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == List.Empty)
	      {
		Expression arg = location.rewriteArg(p2.car, tr);
		Expression loc = location.rewrite(arg, tr);
		Expression[] args = new Expression[2];
		args[0] = new QuoteExp(p1.car);
		args[1] = loc;
		Type[] argTypes = new Type[2];
		argTypes[0] = Compilation.javaStringType;
		argTypes[1] = Compilation.typeProcedure;
		ClassType typeIndirectBinding
		  = ClassType.make("gnu.mapping.IndirectConstraint");
		Method meth = typeIndirectBinding
		  .addMethod("define", argTypes,
			     Scheme.voidType, Access.PUBLIC|Access.STATIC);
		Expression proc = new QuoteExp(new PrimProcedure(meth));
		return new ApplyExp (proc, args);
	      }
	  }
      }
    return tr.syntaxError ("invalid syntax for define-alias");
  }
}
