package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.bytecode.*;

public class object extends Syntax
{
  public Expression rewrite (Object obj, Translator tr)
  {
    tr.mustCompileHere();
    String clname = null;
    if (! (obj instanceof Pair))
      return tr.syntaxError("missing superclass specification in object");
    Pair pair = (Pair) obj;
    if (pair.car instanceof FString)
      {
	clname = pair.car.toString();
	if (! (pair.cdr instanceof Pair))
	  return tr.syntaxError("missing superclass specification after object class name");
	pair = (Pair) pair.cdr;
      }
    int num_supers = List.list_length (pair.car);
    if (num_supers < 0)
      return tr.syntaxError("object superclass specification not a list");
    Expression[] supers = new Expression[num_supers];
    Object superlist = pair.car;
    for (int i = 0;  i < num_supers;  i++)
      {
	Pair superpair = (Pair) superlist;
	supers[i] = tr.rewrite(superpair.car);
	Type type = Scheme.getTypeValue(supers[i]);
	if (type == null || ! (type instanceof ClassType))
	  return tr.syntaxError("object base class/interface not known");
	supers[i] = new QuoteExp(type);
	superlist = superpair.cdr;
      }
    Object components = pair.cdr;
    ObjectExp oexp = new ObjectExp();
    if (clname != null)
      oexp.setName(clname);
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    for (obj = components;  obj != List.Empty;  )
      {
	if (! (obj instanceof Pair)
	    || ! ((pair = (Pair) obj).car instanceof Pair))
	  return tr.syntaxError("object member not a list");
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  {
	    Object type = null;
	    Declaration decl = oexp.addDeclaration((String) pair.car);
	    decl.setSimple(false);
	    if (pair.cdr instanceof Pair)
	      {
		pair = (Pair) pair.cdr;
		Object init = pair.car;
		if (pair.cdr instanceof Pair)
		  {
		    type = init;
		    pair = (Pair) pair.cdr;
		    init = pair.car;
		  }
	      }
	    if (type != null)
	      decl.setType(prim_method.exp2Type(type, tr));
	  }
	else if (pair.car instanceof Pair)
	  {
	    Pair mpair = (Pair) pair.car;
	    if (! (mpair.car instanceof String))
	      return tr.syntaxError("missing method name");
	    String mname = (String) mpair.car;
	    Declaration decl = oexp.addDeclaration(mname);
	    LambdaExp lexp = new LambdaExp();
	    decl.noteValue(lexp);
	    lexp.setName (mname);
	    if (last_method == null)
	      method_list = lexp;
	    else
	      last_method.nextSibling = lexp;
	    last_method = lexp;
	  }
	else
	  return tr.syntaxError("invalid field/method definition");
      }
    oexp.firstChild = method_list;
    oexp.supers = supers;
    tr.push(oexp);
    LambdaExp meth = method_list;
    for (obj = components;  obj != List.Empty;  )
      {
	pair = (Pair) obj;
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  {
	  }
	else if (pair.car instanceof Pair)
	  {
	    Pair mpair = (Pair) pair.car;
	    LambdaExp lexp = meth;
	    meth = meth.nextSibling;
	    Lambda.rewrite(lexp, mpair.cdr, pair.cdr, tr);
	  }
	else
	  return tr.syntaxError("invalid field/method definition");
      }
    tr.pop(oexp);
    return oexp;
  }
}
