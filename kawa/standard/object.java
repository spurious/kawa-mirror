package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.bytecode.*;
import java.util.Vector;

public class object extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (! (form.cdr instanceof Pair))
      return tr.syntaxError("missing superclass specification in object");
    Pair pair = (Pair) form.cdr;
    String clname = null;
    if (pair.car instanceof FString)
      {
	clname = pair.car.toString();
	if (! (pair.cdr instanceof Pair))
	  return tr.syntaxError("missing superclass specification after object class name");
	pair = (Pair) pair.cdr;
      }
    ObjectExp oexp = new ObjectExp();
    // if (clname != null) oexp.setName(clname);
    return rewriteClassDef(pair, oexp, tr);
  }

  public static Expression rewriteClassDef (Pair pair, ObjectExp oexp,
					    Translator tr)
  {
    tr.mustCompileHere();
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
	  return tr.syntaxError("object base class/interface `"+superpair.car
				+"' not known");
	supers[i] = new QuoteExp(type);
	superlist = superpair.cdr;
      }
    Object components = pair.cdr;
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    // First pass (get Declarations).
    Vector inits = null;
    for (Object obj = components;  obj != List.Empty;  )
      {
	if (! (obj instanceof Pair)
	    || ! ((pair = (Pair) obj).car instanceof Pair))
	  return tr.syntaxError("object member not a list");
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  { // Field declaration.
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
		if (inits == null)
		  inits = new Vector (20);
		inits.addElement(decl);
	      }
	    if (type != null)
	      decl.setType(prim_method.exp2Type(type, tr));
	    decl.setCanRead(true);
	    decl.setCanWrite(true);
	  }
	else if (pair.car instanceof Pair)
	  { // Method declaration.
	    Pair mpair = (Pair) pair.car;
	    if (! (mpair.car instanceof String))
	      return tr.syntaxError("missing method name");
	    String mname = (String) mpair.car;
	    Declaration decl = oexp.addDeclaration(mname);
	    LambdaExp lexp = new LambdaExp();
	    lexp.setClassMethod(true);
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

    // Second pass (rewrite method/initializer bodies).
    LambdaExp meth = method_list;
    int init_index = 0;
    for (Object obj = components;  obj != List.Empty;  )
      {
	pair = (Pair) obj;
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  { // Field declaration.
	    Object type = null;
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
		Declaration decl = (Declaration) inits.elementAt(init_index);
		Expression initValue = tr.rewrite(init);
		SetExp sexp = new SetExp (decl.getName(), initValue);
		sexp.binding = decl;
		decl.noteValue(null);
		inits.setElementAt(sexp, init_index++);
	      }
	  }
	else if (pair.car instanceof Pair)
	  { // Method declaration.
	    Pair mpair = (Pair) pair.car;
	    LambdaExp lexp = meth;
	    meth = meth.nextSibling;
 	    Lambda.rewrite(lexp, mpair.cdr, pair.cdr, tr);
	  }
	else
	  return tr.syntaxError("invalid field/method definition");
      }
    if (inits != null)
      {
	int len = inits.size();
	Expression[] assignments = new Expression[len+1];
	inits.copyInto((Object[]) assignments);
	assignments[len] = QuoteExp.voidExp;
	BeginExp bexp = new BeginExp(assignments);
	LambdaExp initMethod = new LambdaExp(bexp);
	tr.push(initMethod);
	initMethod.setName("$finit$");
	initMethod.setClassMethod(true);
	oexp.initMethod = initMethod;
	initMethod.nextSibling = oexp.firstChild;
	oexp.firstChild = initMethod;
	tr.pop(initMethod);
      }
    tr.pop(oexp);
    return oexp;
  }
}
