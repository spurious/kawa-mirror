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
    obj = pair.cdr;
    ObjectExp oexp = new ObjectExp();
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    while (obj != List.Empty)
      {
	if (! (obj instanceof Pair)
	    || ! ((pair = (Pair) obj).car instanceof Pair))
	  return tr.syntaxError("object member not a list");
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  {
	    oexp.addDeclaration((String) pair.car);
	    //return tr.syntaxError("field object member not implemented"); //FIXME
	  }
	else if (pair.car instanceof Pair)
	  {
	    Pair mpair = (Pair) pair.car;
	    if (! (mpair.car instanceof String))
	      return tr.syntaxError("missing method name");
	    String mname = (String) mpair.car;
	    LambdaExp lexp = new LambdaExp();
	    Lambda.rewrite(lexp, mpair.cdr, pair.cdr, tr);
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
    if (clname != null)
      oexp.setName(clname);
    return oexp;
  }
}
