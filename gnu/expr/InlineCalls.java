package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;
import gnu.kawa.util.IdentityHashTable;

public class InlineCalls extends ExpWalker
{
  public static void inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls walker = new InlineCalls(comp);
    walker.walk(exp);
  }

  public InlineCalls (Compilation comp)
  {
    setContext(comp);
  }

  protected Expression walkApplyExp(ApplyExp exp)
  {
    Expression func = exp.func;
    // Replace (apply (lambda (param ...) body ...) arg ...)
    // by: (let ((param arg) ...) body ...).
    // Note this should be done *before* we walk the lambda, so we can
    // walk the body with params bound to the known args.
    if (func instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) func;
        Expression inlined = inlineCall((LambdaExp) func, exp.args, false);
        if (inlined != null)
          return walk(inlined);
      }
    func = walk(func);
    exp.func = func;
    return func.inline(exp, this, null, false);
  }

  /** Walk an ApplyExp assuming function and arguments have been walked. */
  public final Expression walkApplyOnly(ApplyExp exp)
  {
    return exp.func.inline(exp, this, null, true);
  }

  protected Expression walkReferenceExp (ReferenceExp exp)
  {
    Declaration decl = exp.getBinding();
    if (decl != null && decl.field == null && ! decl.getCanWrite())
      {
        Expression dval = decl.getValue();
        if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
          return walkQuoteExp((QuoteExp) dval);
        if (dval instanceof ReferenceExp && ! decl.isAlias())
          {
            ReferenceExp rval = (ReferenceExp) dval;
            Declaration rdecl = rval.getBinding();
            Type dtype = decl.getType();
            if (rdecl != null && ! rdecl.getCanWrite()
                && (dtype == null || dtype == Type.pointer_type
                    // We could also allow (some) widening conversions.
                    || dtype == rdecl.getType())
                && ! rval.getDontDereference())
              return walkReferenceExp(rval);
          }
        if (! exp.isProcedureName()
            && ((decl.flags & Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)
                == Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE))
          {
            // FIXME.  This shouldn't be that hard to fix.  For example,
            // we could treat reference to a one-argument method foo as if
            // were (lambda (x) (foo x)).  Or we could treat it as (this):foo.
            // However, it's a little tricky handling the general case.
            // (What about overloading?  Varargs methods?  Static methods?)  
            comp.error('e', "unimplemented: reference to method "+decl.getName()+" as variable");
            comp.error('e', decl, "here is the definition of ", "");
          }
      }
    return super.walkReferenceExp(exp);
  }

  protected Expression walkIfExp (IfExp exp)
  {
    Expression test = exp.test.walk(this);
    if (test instanceof ReferenceExp)
      {
        Declaration decl = ((ReferenceExp) test).getBinding();
        if (decl != null)
          {
            Expression value = decl.getValue();
            if (value instanceof QuoteExp && value != QuoteExp.undefined_exp)
              test = value;
          }
      }
    if (test instanceof QuoteExp)
      {
        return walk(comp.getLanguage().isTrue(((QuoteExp) test).getValue())
                    ? exp.then_clause
                    : exp.else_clause == null ? QuoteExp.voidExp
                    : exp.else_clause);
      }
    exp.test = test;
    if (exitValue == null)
      exp.then_clause = walk(exp.then_clause);
    if (exitValue == null && exp.else_clause != null)
      exp.else_clause = walk(exp.else_clause);
    return exp;
  }

  protected Expression walkLetExp (LetExp exp)
  {
    Declaration decl = exp.firstDecl();
    int n = exp.inits.length;
    for (int i = 0; i < n; i++, decl = decl.nextDecl())
      {
	Expression init0 = exp.inits[i];
	Expression init = walk(init0);
	exp.inits[i] = init;
	Expression dvalue = decl.value;
	if (dvalue == init0)
	  {
	    decl.value = dvalue = init;
	    if (! decl.getFlag(Declaration.TYPE_SPECIFIED))
	      decl.setType(dvalue.getType());
	  }
      }

    if (exitValue == null)
      exp.body = (Expression) walk(exp.body);
    if (exp.body instanceof ReferenceExp)
      {
        ReferenceExp ref = (ReferenceExp) exp.body;
        Declaration d = ref.getBinding();
        if (d != null && d.context == exp && ! ref.getDontDereference())
          {
            if (n == 1)
              {
                Expression init = exp.inits[0];
                Expression texp = d.getTypeExp();
                if (texp != QuoteExp.classObjectExp)
                  init = walkApplyOnly(Convert.makeCoercion(init, texp));
                return init;
              }
            // Can also optimize if n > 1, but have to check if any
            // other inits can cause side-effects.  Probably not worth it.
          }
      }
    return exp;
  }

  protected Expression walkLambdaExp (LambdaExp exp)
  {
    Declaration firstDecl = exp.firstDecl();
    if (firstDecl != null && firstDecl.isThisParameter()
        && ! exp.isClassMethod() && firstDecl.getType() == null)
      {
        firstDecl.setType(comp.mainClass);
      }
    return walkScopeExp(exp);
  }

  protected Expression walkSetExp (SetExp exp)
  {
    super.walkSetExp(exp);
    Declaration decl = exp.getBinding();
    if (! exp.isDefining() && decl != null
        && ((decl.flags & Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)
            == (Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)))
      comp.error('e', "can't assign to method "+decl.getName(), exp);
    /*
    if (decl != null && ! decl.getFlag(Declaration.TYPE_SPECIFIED))
      {
	// This is a kludge to handle the a #!rest parameter that
	// is implicitly declared to be a Scheme <list>, but may be
	// assinged some other value, which is a legal Scheme idiom.
	// We could set implicitly set the parameter type to <list>,
	// but doing so improves type inference in the common case.
	Type declType = decl.getType();
	if (declType != null && ! exp.new_value.getType().isSubtype(declType))
	  decl.setType(Type.pointer_type);
      }
    */
    return exp;
  }

  /** Attempt to inline a function call.
   * @param lexp function to inline
   * @param args list of actual arguments of function call
   * @param makeCopy true if the body of lexp should of copied; false
   *   if we can re-use lexp because it is no longer needed.
   * @return the inlined expression (a LetExp), or null if we
   *   weren't able to inline.
   */
  public static Expression inlineCall (LambdaExp lexp, Expression[] args,
                                       boolean makeCopy)
  {
    if (lexp.keywords != null
        // Since we re-use the Lambda in-place, we only want to do this
        // for anonymous functions applied directly, as in:
        // (apply (lambda (...) ...) ...)
        || (lexp.nameDecl != null && ! makeCopy))
      return null;
    boolean varArgs = lexp.max_args < 0;
    if ((lexp.min_args == lexp.max_args
         && lexp.min_args == args.length)
        || (varArgs && lexp.min_args == 0))
      {
        Declaration prev = null;
        int i = 0;
        IdentityHashTable mapper;
        Expression[] cargs;
        if (makeCopy)
          {
            mapper = new IdentityHashTable();
            cargs = Expression.deepCopy(args, mapper);
            if (cargs == null && args != null)
              return null;
          }
        else
          {
            mapper = null;
            cargs = args;
          }
        if (varArgs)
          {
            Expression[] xargs = new Expression[args.length+1];
            xargs[0] = QuoteExp.getInstance(lexp.firstDecl().type);
            System.arraycopy(args, 0, xargs, 1, args.length);
            cargs = new Expression[] { new ApplyExp(Invoke.make, xargs) };
          }
        LetExp let = new LetExp(cargs);
        for (Declaration param = lexp.firstDecl(); param != null; i++)
          {
            Declaration next = param.nextDecl();
            if (makeCopy)
              {
                Declaration ldecl = let.addDeclaration(param.symbol, param.type);
                if (param.typeExp != null)
                  {
                    ldecl.typeExp = Expression.deepCopy(param.typeExp);
                    if (ldecl.typeExp == null)
                      return null;
                    
                  }
                mapper.put(param, ldecl);
              }
            else
              {
                lexp.remove(prev, param);
                let.add(prev, param);
              }
            if (! varArgs)
              {
                if ( ! param.getCanWrite())
                  param.setValue(cargs[i]);
              }
            prev = param;
            param = next;
          }
        /*
        for (Declaration param = let.firstDecl(); param != null; )
          {
            Declaration next = param.nextDecl();
            param = next;
          }
        */
        Expression body = lexp.body;
        if (makeCopy)
          {
            body = Expression.deepCopy(body, mapper);
            if (body == null && lexp.body != null)
              return null;
          }
        let.body = body;
        return let;
      }
    /*
    if (lambda.min_args == 0 && lambda.max_args == -1)
      {
        Declaration pargs = lambda.firstDecl();
        Expression[] cargs = Expression.deepCopy(args, mapper);
        Declaration largs = new Declaration
        IdentityHashTable mapper = new IdentityHashTable();
        LetExp let = new LetExp();
        return let;
      }
    if (lambda.min_args != lambda.max_args)
      {
        // FUTURE
      }
    */
    return null;
  }
}
