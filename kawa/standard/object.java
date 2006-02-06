package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import java.util.Vector;
import gnu.mapping.Symbol;
import gnu.bytecode.Type;
import gnu.kawa.functions.Convert;

public class object extends Syntax
{
  public static final object objectSyntax
    = new kawa.standard.object(Scheme.lambda);
  static { objectSyntax.setName("object"); }

  Lambda lambda;
  public static final Keyword accessKeyword = Keyword.make("access");
  public static final Keyword throwsKeyword = Keyword.make("throws");
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
    int num_supers = Translator.listLength(pair.car);
    if (num_supers < 0)
      {
	tr.error('e', "object superclass specification not a list");
	return null;
      }
    Expression[] supers = new Expression[num_supers];
    Object superlist = pair.car;
    for (int i = 0;  i < num_supers;  i++)
      {
	while (superlist instanceof SyntaxForm)
	  {
	    // FIXME - need to pass syntax.
	    superlist = ((SyntaxForm) superlist).form;
	  }
	Pair superpair = (Pair) superlist;
	supers[i] = tr.rewrite_car(superpair, false);
	superlist = superpair.cdr;
      }
    oexp.supers = supers;
    Object components = pair.cdr;
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    // First pass (get Declarations).
    Vector inits = new Vector(20);
    for (Object obj = components;  obj != LList.Empty;  )
      {
	// The SyntaxForm scopes aren't used in scanClassDef, but they are
	// used in rewriteClassDef, and might as well make the code the same.
	while (obj instanceof SyntaxForm)
	  obj = ((SyntaxForm) obj).form;
	if (! (obj instanceof Pair))
	  {
	    tr.error('e', "object member not a list");
	    return null;
	  }
	pair = (Pair) obj;
	Object pair_car = pair.car;
	while (pair_car instanceof SyntaxForm)
	  pair_car = ((SyntaxForm) pair_car).form;
	if (! (pair_car instanceof Pair))
	  {
	    tr.error('e', "object member not a list");
	    return null;
	  }
	obj = pair.cdr; // Next member.
	Object savedPos1 = tr.pushPositionOf(pair);
	pair = (Pair) pair_car;
	pair_car = pair.car;
	while (pair_car instanceof SyntaxForm)
	  pair_car = ((SyntaxForm) pair_car).form;
	if (pair_car instanceof String || pair_car instanceof Symbol
	    || pair_car instanceof Keyword)
	  { // Field declaration.
	    Pair typePair = null;
	    Object sname = pair_car;
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
		Translator.setLine(decl, pair);
		args = pair.cdr;
	      }
	    int nKeywords = 0;
	    boolean seenInit = false;
	    Pair initPair = null;
	    while (args instanceof Pair)
	      {
		pair = (Pair) args;
		Pair keyPair = pair;
		Object key = pair.car;
		Object savedPos2 = tr.pushPositionOf(pair);
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
			if (seenInit)
			  tr.error('e', "duplicate initialization");
			seenInit = true;
			// In the case of 'init-form: EXPR' the scope of EXPR
			// doesn't include this class;
			// in the case of 'init: EXPR' it does.
			if (key != initKeyword)
			  initPair = pair;
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
		else if (args == LList.Empty && ! seenInit)
		  {
		    // CLtL:2 explicitly prohibits this as an extension.
		    initPair = keyPair;
		    seenInit = true;
		  }
		else if (args instanceof Pair
			 && nKeywords == 0 && ! seenInit && typePair == null
			 && (pair = (Pair) args).cdr == LList.Empty)
		  {
		    // Backward compatibility.
		    typePair = keyPair;
		    initPair = pair;
		    args = pair.cdr;
		    seenInit = true;
		  }
		else
		  {
		    args = null;  // Trigger error message
		    break;
		  }
		tr.popPositionOf(savedPos2);
	      }
	    if (args != LList.Empty)
	      {
		tr.error('e', "invalid argument list for slot '"
			 + sname + '\''+" args:"+(args==null?"null":args.getClass().getName()));
		return null;
	      }
	    if (seenInit)
	      {
		boolean isStatic
		  = allocationFlag == Declaration.STATIC_SPECIFIED;
		inits.addElement(decl != null ? (Object) decl
				 : isStatic ? Boolean.TRUE : Boolean.FALSE);
		inits.addElement(initPair);
	      }
	    if (decl == null)
	      {
		if (! seenInit)
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
	else if (pair_car instanceof Pair)
	  { // Method declaration.
	    Pair mpair = (Pair) pair_car;
	    Object mname = mpair.car;
	    if (! (mname instanceof String)
		&& ! (mname instanceof Symbol))
	      {
		tr.error('e', "missing method name");
		return null;
	      }
	    Declaration decl
              = oexp.addDeclaration(mname, Compilation.typeProcedure);
	    Translator.setLine(decl, mpair);
	    LambdaExp lexp = new LambdaExp();
	    lexp.outer = oexp;
	    lexp.setClassMethod(true);
	    decl.noteValue(lexp);
	    decl.setFlag(Declaration.FIELD_OR_METHOD);
	    decl.setProcedureDecl(true);
	    lexp.setSymbol(mname);
	    if (last_method == null)
	      method_list = lexp;
	    else
	      last_method.nextSibling = lexp;
	    last_method = lexp;
	  }
	else
	  tr.error ('e', "invalid field/method definition");
	tr.popPositionOf(savedPos1);
      }
    Object[] result = {
      oexp,
      components,
      inits,
      method_list
    };
    return result;
  }

  public void rewriteClassDef (Object[] saved, Translator tr)
  {
    ClassExp oexp = (ClassExp) saved[0];
    Object components = saved[1];
    Vector inits = (Vector) saved[2];
    LambdaExp method_list = (LambdaExp) saved[3];
    oexp.firstChild = method_list;

    oexp.setClassName(tr);

    // First a pass over init-form: specifiers, since these are evaluated
    // in a scope outside the current class.
    int len = inits.size();
    for (int i = 0;  i < len;  i += 2)
      {
	Object init = inits.elementAt(i+1);
	if (init != null)
          rewriteInit(inits.elementAt(i), oexp, (Pair) init, tr, null);
      }
    
    tr.push(oexp);

    // Pass to rewrite method/initializer bodies.
    LambdaExp meth = method_list;
    int init_index = 0;  // Input index in inits Vector.
    SyntaxForm componentsSyntax = null;
    for (Object obj = components;  obj != LList.Empty;  )
      {
	while (obj instanceof SyntaxForm)
	  {
	    componentsSyntax = (SyntaxForm) obj;
	    obj = componentsSyntax.form;
	  }
	Pair pair = (Pair) obj;
	Object savedPos1 = tr.pushPositionOf(pair);
	Object pair_car = pair.car;
	SyntaxForm memberSyntax = componentsSyntax;
	while (pair_car instanceof SyntaxForm)
	  {
	    memberSyntax = (SyntaxForm) pair_car;
	    pair_car = memberSyntax.form;
	  }
	try
	  {
	    obj = pair.cdr; // Next member.
	    pair = (Pair) pair_car;
	    pair_car = pair.car;
	    SyntaxForm memberCarSyntax = memberSyntax;
	    while (pair_car instanceof SyntaxForm)
	      {
		memberCarSyntax = (SyntaxForm) pair_car;
		pair_car = memberCarSyntax.form;
	      }
	    if (pair_car instanceof String || pair_car instanceof Symbol
		|| pair_car instanceof Keyword)
	      { // Field declaration.
		Object type = null;
		int nKeywords = 0;
		Object args = pair_car instanceof Keyword ? pair : pair.cdr;
		Pair initPair = null;
		while (args instanceof Pair)
		  {
		    pair = (Pair) args;
		    Object key = pair.car;
		    Object savedPos2 = tr.pushPositionOf(pair);
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
			    initPair = pair;
			  }
			else
			  {
			    // handled in first pass.
			  }
		      }
		    else if (args == LList.Empty && initPair == null)
		      {
			// CLtL:2 explicitly prohibits this as an extension.
			initPair = pair;
		      }
		    else if (args instanceof Pair && nKeywords == 0
			     && initPair == null && type == null
			     && (pair = (Pair) args).cdr == LList.Empty)
		      {
			// Backward compatibility.
			type = key;
			initPair = pair;
			args = pair.cdr;
		      }
		    else
		      {
			args = null;  // Trigger error message
			break;
		      }
		    tr.popPositionOf(savedPos2);
		  }
		if (initPair != null)
		  {
		    Object d = inits.elementAt(init_index++);
		    boolean isStatic = d instanceof Declaration
                      ? ((Declaration) d).getFlag(Declaration.STATIC_SPECIFIED)
                      : d == Boolean.TRUE;
		    if (inits.elementAt(init_index++) == null)
                      rewriteInit(d, oexp, initPair, tr, memberSyntax);
		  }
	      }
	    else if (pair_car instanceof Pair)
	      { // Method declaration.
		ScopeExp save_scope = tr.currentScope();
		// If we saw a TemplateScope (in a SyntaxForm) specific to the
		// formal parameters,  pass it to rewrite so it can create a
		// renamed alias.  A TemplateScope that covers the formals
		// *and* the body we handle using setCurrentScope.
		if (memberSyntax != null)
		  tr.setCurrentScope(memberSyntax.scope);
                if ("*init*".equals(meth.getName()))
                  {
                    meth.setReturnType(Type.void_type);
                    if (! oexp.isSimple())
                      tr.error('e', "'*init*' methods only supported for simple classes");
                  }
                Translator.setLine(meth, pair);
                LambdaExp saveLambda = tr.curMethodLambda;
                tr.curMethodLambda = meth;
		lambda.rewrite(meth, ((Pair) pair_car).cdr, pair.cdr, tr,
			       memberCarSyntax != null
			       && (memberSyntax == null
				   || memberCarSyntax.scope != memberSyntax.scope)
			       ? memberCarSyntax.scope
			       : null);
                tr.curMethodLambda = saveLambda;
		if (memberSyntax != null)
		  tr.setCurrentScope(save_scope);
		meth = meth.nextSibling;
	      }
	    else
	      tr.syntaxError("invalid field/method definition");
	  }
	finally
	  {
	    tr.popPositionOf(savedPos1);
	  }
	
      }
    // If initMethod/clinitMethod were created by the "outer" (first) call
    // to rewriteInit, then we may need to fix up their outer chain.
    if (oexp.initMethod != null)
      oexp.initMethod.outer = oexp;
    if (oexp.clinitMethod != null)
      oexp.clinitMethod.outer = oexp;
    tr.pop(oexp);
    oexp.setTypes(tr);
    oexp.declareParts(tr);
  }

  private static void rewriteInit (Object d, ClassExp oexp, Pair initPair,
                                   Translator tr, SyntaxForm memberSyntax)
  {
    boolean isStatic = d instanceof Declaration
      ? ((Declaration) d).getFlag(Declaration.STATIC_SPECIFIED)
      : d == Boolean.TRUE;
    LambdaExp initMethod = isStatic ? oexp.clinitMethod : oexp.initMethod;
    if (initMethod == null)
      {
        initMethod = new LambdaExp(new BeginExp());        
        initMethod.setClassMethod(true);
        if (isStatic)
          {
            initMethod.setName("$clinit$");
            oexp.clinitMethod = initMethod;
          }
        else
          {
            initMethod.setName("$finit$");
            oexp.initMethod = initMethod;
            // pseudo-this??  $finit$ is a static method - but (this) is valid.
            // Is type getting set?  FIXME
            initMethod.add(null, new Declaration(ThisExp.THIS_NAME));
          }
        initMethod.nextSibling = oexp.firstChild;
        oexp.firstChild = initMethod;
      }
    tr.push(initMethod);
    LambdaExp saveLambda = tr.curMethodLambda;
    tr.curMethodLambda = initMethod;
    Expression initValue = tr.rewrite_car(initPair, memberSyntax);
    if (d instanceof Declaration)
      {
        Declaration decl = (Declaration) d;
        SetExp sexp = new SetExp(decl, initValue);
        sexp.setFile(decl.getFile());
        sexp.setLine(decl.getLine(), decl.getColumn());
        decl.noteValue(null);
        initValue = sexp;
      }
    else
      initValue = Convert.makeCoercion(initValue, new QuoteExp(Type.void_type));
    ((BeginExp) initMethod.body).add(initValue);
    tr.curMethodLambda = saveLambda;
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
