package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import gnu.bytecode.*;
import java.util.Vector;
import gnu.mapping.Symbol;

public class object extends Syntax
{
  Lambda lambda;
  public static final Keyword accessKeyword = Keyword.make("access");
  static final Keyword typeKeyword = Keyword.make("type");
  public static final Keyword allocationKeyword = Keyword.make("allocation");
  static final Keyword initKeyword = Keyword.make("init");
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
    Object[] saved = scanClassDef(pair, oexp, tr);
    if (saved != null)
      rewriteClassDef(saved, tr);
    return oexp;
  }

  /** Does the first "scan-time" processing of the class/object definition.
   * Returns an array of values to be used at "rewrite-time".
   */
  public Object[] scanClassDef (Pair pair, ClassExp oexp, Translator tr)
  {
    tr.mustCompileHere();
    int num_supers = LList.listLength(pair.car, false);
    if (num_supers < 0)
      {
	tr.error('e', "object superclass specification not a list");
	return null;
      }
    Expression[] supers = new Expression[num_supers];
    Object superlist = pair.car;
    for (int i = 0;  i < num_supers;  i++)
      {
	Pair superpair = (Pair) superlist;
	supers[i] = tr.rewrite(superpair.car);
	superlist = superpair.cdr;
      }
    oexp.supers = supers;
    Object components = pair.cdr;
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    // First pass (get Declarations).
    // Should be done at scan time.  FIXME.
    Vector inits = new Vector(20);
    Vector clinits = new Vector(20);
    for (Object obj = components;  obj != LList.Empty;  )
      {
	if (! (obj instanceof Pair)
	    || ! ((pair = (Pair) obj).car instanceof Pair))
	  {
	    tr.error('e', "object member not a list");
	    return null;
	  }
	obj = pair.cdr; // Next member.
	pair = (Pair) pair.car;
	if (pair.car instanceof String || pair.car instanceof Symbol
	    || pair.car instanceof Keyword)
	  { // Field declaration.
	    Pair typePair = null;
	    Object sname = pair.car;
	    Object args;
	    Declaration decl;
	    int allocationFlag = 0;
	    String accessFlagName = null;
	    int accessFlag = 0;
	    if (sname instanceof Keyword)
	      {
		decl = null;
		args = pair;
	      }
	    else
	      {
		decl = oexp.addDeclaration(sname);
		decl.setSimple(false);
		decl.setFlag(Declaration.FIELD_OR_METHOD);
		args = pair.cdr;
	      }
	    int nKeywords = 0;
	    Object init = null;
	    Expression initExp = null;
	    while (args instanceof Pair)
	      {
		pair = (Pair) args;
		Pair keyPair = pair;
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
		      typePair = pair;
		    else if (key == allocationKeyword)
		      {
			if (allocationFlag != 0)
			  tr.error('e', "duplicate allocation: specification");
			if (matches(value, "class", tr)
			    || matches(value, "static", tr))
			  allocationFlag = Declaration.STATIC_SPECIFIED;
			else if (matches(value, "instance", tr))
			  allocationFlag = Declaration.NONSTATIC_SPECIFIED;
			else
			  tr.error('e', "unknown allocation kind '"+value+"'");
		      }
		    else if (key == initKeyword
			     || key == initformKeyword
			     || key == init_formKeyword
			     || key == init_valueKeyword)
		      {
			init = value;
			// In the case of 'init-form: EXPR' the scope of EXPR
			// doesn't include this class;
			// in the case of 'init: EXPR' it does.
			if (key != initKeyword)
			  initExp = tr.rewrite(init);
		      }
		    else if (key == init_keywordKeyword)
		      {
			if (! (value instanceof Keyword))
			  tr.error('e', "invalid 'init-keyword' - not a keyword");
			else if (((Keyword) value).getName()
				 != sname.toString())
			  tr.error('w', "init-keyword option ignored");
		      }
		    else if (key == accessKeyword)
		      {
			String newAccessFlag = null;
			if (matches(value, "private", tr))
			  {
			    newAccessFlag = "private";
			    accessFlag = Declaration.PRIVATE_ACCESS;
			  }
			else if (matches(value, "protected", tr))
			  {
			    newAccessFlag = "protected";
			    accessFlag = Declaration.PROTECTED_ACCESS;
			  }
			else if (matches(value, "public", tr))
			  {
			    newAccessFlag = "public";
			    accessFlag = Declaration.PUBLIC_ACCESS;
			  }
			else if (matches(value, "package", tr))
			  {
			    newAccessFlag = "package";
			    accessFlag = Declaration.PACKAGE_ACCESS;
			  }
			else
			  {
			    tr.error('e', "unknown access specifier");
			  }
			if (accessFlagName != null && newAccessFlag != null)
			  {
			    tr.error('e', "duplicate access specifiers - "
				     + accessFlagName + " and "
				     + newAccessFlag);
			  }
			accessFlagName = newAccessFlag;
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
			 && nKeywords == 0 && init == null && typePair == null
			 && (pair = (Pair) args).cdr == LList.Empty)
		  {
		    // Backward compatibility.
		    typePair = keyPair;
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
	      {
		tr.error('e', "invalid argument list for slot '"
			 + sname + '\''+" args:"+(args==null?"null":args.getClass().getName()));
		return null;
	      }
	    if (init != null)
	      {
		boolean isStatic
		  = allocationFlag == Declaration.STATIC_SPECIFIED;
		inits.addElement(decl != null ? (Object) decl
				 : isStatic ? Boolean.TRUE : Boolean.FALSE);
		inits.addElement(initExp);
	      }
	    if (decl == null)
	      {
		if (init == null)
		  {
		    tr.error('e', "missing field name");
		    return null;
		  }
	      }
	    else
	      {
		if (typePair != null)
		  decl.setType(tr.exp2Type(typePair));
		if (allocationFlag != 0)
		  decl.setFlag(allocationFlag);
		if (accessFlag != 0)
		  decl.setFlag(accessFlag);
		decl.setCanRead(true);
		decl.setCanWrite(true);
	      }
	  }
	else if (pair.car instanceof Pair)
	  { // Method declaration.
	    Pair mpair = (Pair) pair.car;
	    Object mname = mpair.car;
	    if (! (mname instanceof String)
		&& ! (mname instanceof Symbol))
	      {
		tr.error('e', "missing method name");
		return null;
	      }
	    Declaration decl = oexp.addDeclaration(mname);
	    LambdaExp lexp = new LambdaExp();
	    lexp.outer = oexp;
	    lexp.setClassMethod(true);
	    decl.noteValue(lexp);
	    decl.setFlag(Declaration.FIELD_OR_METHOD);
	    decl.setProcedureDecl(true);
	    lexp.setName (mname);
	    if (pair.cdr instanceof PairWithPosition)
	      lexp.setFile(((PairWithPosition) pair.cdr).getFile());
	    Object body = lambda.rewriteAttrs(lexp, pair.cdr, tr);
	    lambda.rewrite(lexp, mpair.cdr, tr);
	    if (last_method == null)
	      method_list = lexp;
	    else
	      last_method.nextSibling = lexp;
	    last_method = lexp;
	  }
	else
	  tr.error ('e', "invalid field/method definition");
      }
    Object[] result = {
      oexp,
      components,
      inits,
      clinits,
      method_list
    };
    return result;
  }

  public void rewriteClassDef (Object[] saved, Translator tr)
  {
    ClassExp oexp = (ClassExp) saved[0];
    Object components = saved[1];
    Vector inits = (Vector) saved[2];
    Vector clinits = (Vector) saved[3];
    LambdaExp method_list = (LambdaExp) saved[4];
    oexp.firstChild = method_list;
    tr.push(oexp);

    // Second pass (rewrite method/initializer bodies).
    LambdaExp meth = method_list;
    int init_index = 0;  // Input index in inits Vector.
    int finit_index = 0;   // Output index in inits vector.
    for (Object obj = components;  obj != LList.Empty;  )
      {
	Pair pair = (Pair) obj;
	Object savedPos = tr.pushPositionOf(pair);
	try
	  {
	    obj = pair.cdr; // Next member.
	    pair = (Pair) pair.car;
	    if (pair.car instanceof String || pair.car instanceof Symbol
		|| pair.car instanceof Keyword)
	      { // Field declaration.
		Object type = null;
		int nKeywords = 0;
		Object args = pair.car instanceof Keyword ? pair : pair.cdr;
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
			else if (key == initKeyword
				 || key == initformKeyword
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
		    boolean isStatic;
		    Object d = inits.elementAt(init_index++);
		    Expression initValue
		      = (Expression) inits.elementAt(init_index++);
		    if (initValue == null)
		      initValue = tr.rewrite(init);
		    if (d instanceof Declaration)
		      {
			Declaration decl = (Declaration) d;
			isStatic = decl.getFlag(Declaration.STATIC_SPECIFIED);
			SetExp sexp = new SetExp (decl.getName(), initValue);
			sexp.binding = decl;
			decl.noteValue(null);
			initValue = sexp;
		      }
		    else
		      {
			isStatic = d == Boolean.TRUE;
		      }
		    if (isStatic)
		      clinits.addElement(initValue);
		    else
		      inits.setElementAt(initValue, finit_index++);
		  }
	      }
	    else if (pair.car instanceof Pair)
	      { // Method declaration.
		Pair mpair = (Pair) pair.car;
		LambdaExp lexp = meth;
		meth = meth.nextSibling;
		Object body = lambda.skipAttrs(lexp, pair.cdr, tr);
		lambda.rewriteBody(lexp, body, tr);
	      }
	    else
	      tr.syntaxError("invalid field/method definition");
	  }
	finally
	  {
	    tr.popPositionOf(savedPos);
	  }
	
      }
    inits.setSize(finit_index);
    makeInitMethod(inits, false, oexp, tr);
    makeInitMethod(clinits, true, oexp, tr);
    tr.pop(oexp);
  }

  private static void makeInitMethod (Vector inits, boolean isStatic,
				      ClassExp oexp, Translator tr)
  {
    int len = inits.size();
    if (len == 0)
      return;
    Expression[] assignments = new Expression[len+1];
    inits.copyInto((Object[]) assignments);
    assignments[len] = QuoteExp.voidExp;
    BeginExp bexp = new BeginExp(assignments);
    LambdaExp initMethod = new LambdaExp(bexp);
    initMethod.setClassMethod(true);
    tr.push(initMethod);
    if (isStatic)
      {
	initMethod.setName("$clinit$");
	oexp.clinitMethod = initMethod;
      }
    else
      {
	initMethod.setName("$finit$");
	oexp.initMethod = initMethod;
      }
    initMethod.nextSibling = oexp.firstChild;
    oexp.firstChild = initMethod;
    tr.pop(initMethod);
  }

  /** True if <code>exp</code> matches <code>tag:</code>, <code>"tag"</code>,
   * or <code>'tag</code>.  The latter is recommended as a matter of style.
   */
  static boolean matches (Object exp, String tag, Translator tr)
  {
    String value;
    Pair pair;
    if (exp instanceof Keyword)
      value = ((Keyword) exp).getName();
    else if (exp instanceof FString)
      value = ((FString) exp).toString();
    else if (exp instanceof Pair
	     && tr.matches((pair = (Pair) exp).car, Scheme.quote_sym)
	     && pair.cdr instanceof Pair
	     && (pair = (Pair) pair.cdr).cdr == LList.Empty
	     && pair.car instanceof String)
      value = (String) pair.car;
    else
      return false;
    return tag == null || tag.equals(value);
  }
}
