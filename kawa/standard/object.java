package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import gnu.bytecode.*;
import java.util.Vector;

public class object extends Syntax
{
  Lambda lambda;
  static final Keyword typeKeyword = Keyword.make("type");
  static final Keyword allocationKeyword = Keyword.make("allocation");
  static final Keyword instanceKeyword = Keyword.make("instance");
  static final Keyword classKeyword = Keyword.make("class");
  static final Keyword initformKeyword = Keyword.make("initform");
  static final Keyword init_formKeyword = Keyword.make("init-form");
  static final Keyword init_valueKeyword = Keyword.make("init-value");
  static final Keyword init_keywordKeyword = Keyword.make("init-keyword");

  public object(Lambda lambda)
  {
    this.lambda = lambda;
  }

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
    oexp.setSimple(true);
    // if (clname != null) oexp.setName(clname);
    return rewriteClassDef(pair, oexp, tr);
  }

  public Expression rewriteClassDef (Pair pair, ClassExp oexp,
                                     Translator tr)
  {
    tr.mustCompileHere();
    int num_supers = LList.listLength(pair.car, false);
    if (num_supers < 0)
      return tr.syntaxError("object superclass specification not a list");
    Expression[] supers = new Expression[num_supers];
    Object superlist = pair.car;
    for (int i = 0;  i < num_supers;  i++)
      {
	Pair superpair = (Pair) superlist;
	supers[i] = tr.rewrite(superpair.car);
	superlist = superpair.cdr;
      }
    Object components = pair.cdr;
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    // First pass (get Declarations).
    // Should be done at scan time.  FIXME.
    Vector inits = null;
    for (Object obj = components;  obj != LList.Empty;  )
      {
	if (! (obj instanceof Pair)
	    || ! ((pair = (Pair) obj).car instanceof Pair))
	  return tr.syntaxError("object member not a list");
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  { // Field declaration.
	    Object type = null;
	    String sname = (String) pair.car;
	    Declaration decl = oexp.addDeclaration(sname);
	    decl.setSimple(false);
	    int nKeywords = 0;
	    Object args = pair.cdr;
	    Object init = null;
	    while (args instanceof Pair)
	      {
		pair = (Pair) args;
		Object key = pair.car;
		args = pair.cdr;
		if ((key == "::" || key instanceof Keyword)
		    && args instanceof Pair)
		  {
		    nKeywords++;
		    pair = (Pair) args;
		    Object value = pair.car;
		    args = pair.cdr;
		    if (key == "::" || key == typeKeyword)
		      type = value;
		    else if (key == allocationKeyword)
		      {
			if (value == classKeyword)
			  decl.setFlag(Declaration.STATIC_SPECIFIED);
			else if (value == instanceKeyword)
			  decl.setFlag(Declaration.NONSTATIC_SPECIFIED);
			else
			  tr.error('e', "unknown allocation kind '"+value+"'");
		      }
		    else if (key == initformKeyword
			     || key == init_formKeyword
			     || key == init_valueKeyword)
		      {
			init = value;
		      }
		    else if (key == init_keywordKeyword)
		      {
			if (! (value instanceof Keyword))
			  tr.error('e', "invalid 'init-keyword' - not a keyword");
			else if (((Keyword) value).getName() != sname)
			  tr.error('w', "init-keyword option ignored");
		      }
		    else
		      {
			tr.error('w', "unknown slot keyword '"+key+"'");
		      }
		  }
		else if (args == LList.Empty && init == null)
		  {
		    // CLtL:2 explicitly prohibits this as an extension.
		    init = key;
		  }
		else if (args instanceof Pair
			 && nKeywords == 0 && init == null && type == null
			 && (pair = (Pair) args).cdr == LList.Empty)
		  {
		    // Backward compatibility.
		    type = key;
		    init = pair.car;
		    args = pair.cdr;
		  }
		else
		  {
		    args = null;  // Trigger error message
		    break;
		  }
	      }
	    if (args != LList.Empty)
	      return tr.syntaxError("invalid argument list for slot '"
				    + sname + '\''+" args:"+(args==null?"null":args.getClass().getName()));
	    if (init != null)
	      {
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
    for (Object obj = components;  obj != LList.Empty;  )
      {
	pair = (Pair) obj;
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String)
	  { // Field declaration.
	    Object type = null;
	    int nKeywords = 0;
	    Object args = pair.cdr;
	    Object init = null;
	    while (args instanceof Pair)
	      {
		pair = (Pair) args;
		Object key = pair.car;
		args = pair.cdr;
		if ((key == "::" || key instanceof Keyword)
		    && args instanceof Pair)
		  {
		    nKeywords++;
		    pair = (Pair) args;
		    Object value = pair.car;
		    args = pair.cdr;
		    if (key == "::" || key == typeKeyword)
		      type = value;
		    else if (key == initformKeyword
			     || key == init_formKeyword
			     || key == init_valueKeyword)
		      {
			init = value;
		      }
		    else
		      {
			// handled in first pass.
		      }
		  }
		else if (args == LList.Empty && init == null)
		  {
		    // CLtL:2 explicitly prohibits this as an extension.
		    init = key;
		  }
		else if (args instanceof Pair
			 && nKeywords == 0 && init == null && type == null
			 && (pair = (Pair) args).cdr == LList.Empty)
		  {
		    // Backward compatibility.
		    type = key;
		    init = pair.car;
		    args = pair.cdr;
		  }
		else
		  {
		    args = null;  // Trigger error message
		    break;
		  }
	      }
	    if (init != null)
	      {
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
 	    lambda.rewrite(lexp, mpair.cdr, pair.cdr, tr);
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
