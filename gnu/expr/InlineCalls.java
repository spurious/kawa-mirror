// Copyright (c) 2010  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;
import gnu.kawa.util.IdentityHashTable;
import gnu.mapping.*;
import gnu.math.IntNum;
import java.lang.reflect.InvocationTargetException;
/* #ifdef use:java.dyn */
// import java.dyn.*;
/* #endif */

/**
 * The main Expression re-writing pass.
 * This pass handles type-checking (work in progress).
 * Also checks for calls to known Procedures, and may call
 * a procedure-specific handler, which may do inlining, constant-folding,
 * error-checking, and general munging.
 *
 * Should perhaps rename to something like "Validate" since
 * we do type-checking and other stuff beyond inlining.
 */

public class InlineCalls extends ExpExpVisitor<Type>
{
  public static Expression inlineCalls (Expression exp, Compilation comp)
  {
    InlineCalls visitor = new InlineCalls(comp);
    return visitor.visit(exp, null);
  }

  public InlineCalls (Compilation comp)
  {
    setContext(comp);
  }

  public Expression visit (Expression exp, Type required)
  {
    if (! exp.getFlag(Expression.VALIDATED))
      {
        exp.setFlag(Expression.VALIDATED); // Protect againts cycles.
        exp = super.visit(exp, required);
        exp.setFlag(Expression.VALIDATED);
      }
    return checkType(exp, required);
  }

  public Expression checkType(Expression exp, Type required)
  {
    Type expType = exp.getType();
    boolean incompatible;
    if (required instanceof ClassType && ((ClassType) required).isInterface()
        && expType.isSubtype(Compilation.typeProcedure)
        && ! expType.isSubtype(required))
      {
        if (exp instanceof LambdaExp)
          {
            Method amethod = ((ClassType) required).checkSingleAbstractMethod();
            if (amethod != null)
              {
                LambdaExp lexp = (LambdaExp) exp;
                ObjectExp oexp = new ObjectExp();
                oexp.setLocation(exp);
                oexp.supers = new Expression[] { new QuoteExp(required) };
                oexp.setTypes(getCompilation());
                Object mname = amethod.getName();
                oexp.addMethod(lexp, mname);
                Declaration mdecl = oexp.addDeclaration(mname, Compilation.typeProcedure);
                oexp.firstChild = lexp;
                oexp.declareParts(comp);
                return visit(oexp, required);
             }
          }
        incompatible = true;
      }
    else
      {
        if (expType == Type.toStringType)
          expType = Type.javalangStringType;
        incompatible = required != null && required.compare(expType) == -3;
        if (incompatible && required instanceof TypeValue)
          {
            Expression converted = ((TypeValue) required).convertValue(exp);
            if (converted != null)
              return converted;
          }
      }
    if (incompatible)
      {
        Language language = comp.getLanguage();
        comp.error('w', "type "+(language.formatType(expType)
                                 +" is incompatible with required type "
                                 +language.formatType(required)),
                   exp);
      }
    return exp;
  }

  protected Expression visitApplyExp(ApplyExp exp, Type required)
  {
    Expression func = exp.func;
    // Replace (apply (lambda (param ...) body ...) arg ...)
    // by: (let ((param arg) ...) body ...).
    // Note this should be done *before* we visit the lambda, so we can
    // visit the body with params bound to the known args.
    if (func instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) func;
        Expression inlined = inlineCall((LambdaExp) func, exp.args, false);
        if (inlined != null)
          return visit(inlined, required);
      }
    func = visit(func, null);
    exp.func = func;
    return func.validateApply(exp, this, required, null);
  }

  /** Visit an ApplyExp assuming function and arguments have been visited. */
  public final Expression visitApplyOnly(ApplyExp exp, Type required)
  {
    return exp.func.validateApply(exp, this, required, null);
  }

  public static Integer checkIntValue (Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        QuoteExp qarg = (QuoteExp) exp;
        Object value = qarg.getValue();
        if (! qarg.isExplicitlyTyped() && value instanceof IntNum)
          {
            IntNum ivalue = (IntNum) value;
            if (ivalue.inIntRange())
              return Integer.valueOf(ivalue.intValue());
          }
      }
    return null;
  }

  public static Long checkLongValue (Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        QuoteExp qarg = (QuoteExp) exp;
        Object value = qarg.getValue();
        if (! qarg.isExplicitlyTyped() && value instanceof IntNum)
          {
            IntNum ivalue = (IntNum) value;
            if (ivalue.inLongRange())
              return Long.valueOf(ivalue.longValue());
          }
      }
    return null;
  }

  public QuoteExp fixIntValue (Expression exp)
  {
    Integer ival = InlineCalls.checkIntValue(exp);
    if (ival != null)
      return new QuoteExp(ival, comp.getLanguage().getTypeFor(Integer.TYPE));
    return null;
  }

  public QuoteExp fixLongValue (Expression exp)
  {
    Long ival = InlineCalls.checkLongValue(exp);
    if (ival != null)
      return new QuoteExp(ival, comp.getLanguage().getTypeFor(Long.TYPE));
    return null;
  }

  protected Expression visitQuoteExp (QuoteExp exp, Type required)
  {
    Object value;
    if (exp.getRawType() == null && ! exp.isSharedConstant()
        && (value = exp.getValue()) != null)
      {
        Language language = comp.getLanguage();
        Type vtype = language.getTypeFor(value.getClass());
        if (vtype == Type.toStringType)
          vtype = Type.javalangStringType;
        exp.type = vtype;
        if (required instanceof PrimType)
          {
            char sig1 = required.getSignature().charAt(0);
            QuoteExp ret = sig1 == 'I' ? fixIntValue(exp)
              : sig1 == 'J'? fixLongValue(exp) : null;
            if (ret != null)
              exp = ret;
          }
      }
    return exp;
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Type required)
  {
    Declaration decl = exp.getBinding();
    if (decl != null && decl.field == null && ! decl.getCanWrite())
      {
        Expression dval = decl.getValue();
        if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
          return visitQuoteExp((QuoteExp) dval, required);
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
              return visitReferenceExp(rval, required);
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
    return super.visitReferenceExp(exp, required);
  }

  protected Expression visitIfExp (IfExp exp, Type required)
  {
    Expression test = exp.test.visit(this, null);
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
    exp.test = test;
    if (exitValue == null)
      exp.then_clause = visit(exp.then_clause, required);
    if (exitValue == null && exp.else_clause != null)
      exp.else_clause = visit(exp.else_clause, required);
    if (test instanceof QuoteExp)
      {
        boolean truth = comp.getLanguage().isTrue(((QuoteExp) test).getValue());
        return exp.select(truth);
      }
    if (test.getType().isVoid())
      {
        boolean truth = comp.getLanguage().isTrue(Values.empty);
        comp.error('w', "void-valued condition is always "+truth);
        return new BeginExp(test, exp.select(truth));
      }
    return exp;
  }

  protected Expression visitBeginExp (BeginExp exp, Type required)
  {
    int last = exp.length - 1;
    for (int i = 0;  i <= last;  i++)
      {
        exp.exps[i] = visit(exp.exps[i], i < last ? null : required);
      }
    return exp;
  }

  protected Expression visitScopeExp (ScopeExp exp, Type required)
  {
    exp.visitChildren(this, null);
    visitDeclarationTypes(exp);
    for (Declaration decl = exp.firstDecl();  decl != null;
         decl = decl.nextDecl())
      {
        if (decl.type == null)
          {
            Expression val = decl.getValue();
            decl.type = Type.objectType;
            decl.setType(val != null && val != QuoteExp.undefined_exp
                         ? val.getType()
                         : Type.objectType);
         }
      }
    return exp;
  }

  protected Expression visitLetExp (LetExp exp, Type required)
  {
    Declaration decl = exp.firstDecl();
    int n = exp.inits.length;
    for (int i = 0; i < n; i++, decl = decl.nextDecl())
      {
	Expression init0 = exp.inits[i];
        boolean typeSpecified = decl.getFlag(Declaration.TYPE_SPECIFIED);
        Type dtype = typeSpecified && init0 != QuoteExp.undefined_exp ? decl.getType() : null;
	Expression init = visit(init0, dtype);
	exp.inits[i] = init;
	Expression dvalue = decl.value;
	if (dvalue == init0)
	  {
	    decl.value = dvalue = init;
	    if (! typeSpecified)
	      decl.setType(dvalue.getType());
	  }
      }

    if (exitValue == null)
      exp.body = visit(exp.body, required);
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
                // Note this optimization does yield worse error messages
                // than using CheckedTarget.  FIXME.
                if (texp != QuoteExp.classObjectExp)
                  init = visitApplyOnly(Compilation.makeCoercion(init, texp), null);
                return init;
              }
            // Can also optimize if n > 1, but have to check if any
            // other inits can cause side-effects.  Probably not worth it.
          }
      }
    return exp;
  }

  protected Expression visitLambdaExp (LambdaExp exp, Type required)
  {
    Declaration firstDecl = exp.firstDecl();
    if (firstDecl != null && firstDecl.isThisParameter()
        && ! exp.isClassMethod() && firstDecl.type == null)
      {
        firstDecl.setType(comp.mainClass);
      }
    return visitScopeExp(exp, required);
  }

  protected Expression visitTryExp (TryExp exp, Type required)
  {
    if (exp.getCatchClauses() == null && exp.getFinallyClause() == null)
      return visit(exp.try_clause, required);
    else
      return super.visitTryExp(exp, required);
  }

  protected Expression visitSetExpValue (Expression new_value, Type required,
                                         Declaration decl)
  {
    return visit(new_value, decl == null || decl.isAlias() ? null : decl.type);
  }

  protected Expression visitSetExp (SetExp exp, Type required)
  {
    Declaration decl = exp.getBinding();
    super.visitSetExp(exp, required);
    if (! exp.isDefining() && decl != null
        && ((decl.flags & Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)
            == (Declaration.FIELD_OR_METHOD+Declaration.PROCEDURE)))
      comp.error('e', "can't assign to method "+decl.getName(), exp);
    if (decl != null && decl.getFlag(Declaration.TYPE_SPECIFIED))
      {
        if (CompileReflect.checkKnownClass(decl.getType(), comp) < 0)
          decl.setType(Type.errorType);
      }
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

  /* #ifdef use:java.dyn */
  // static final java.dyn.MethodType validateApplyMType =
  //   java.dyn.MethodType.make(gnu.expr.Expression.class,
  //                            gnu.expr.ApplyExp.class,
  //                            gnu.expr.InlineCalls.class,
  //                            gnu.bytecode.Type.class,
  //                            gnu.mapping.Procedure.class);
  /* #else */
  private static Class[] inlinerMethodArgTypes;
  private static synchronized Class[] getInlinerMethodArgTypes()
    throws Exception
  {
    Class[] t = inlinerMethodArgTypes;
    if (t == null)
      {
        t = new Class[] { Class.forName("gnu.expr.ApplyExp"),
                         Class.forName("gnu.expr.InlineCalls"),
                         Class.forName("gnu.bytecode.Type"),
                         Class.forName("gnu.mapping.Procedure") };
        inlinerMethodArgTypes = t;
      }
    return t;
  }
  /* #endif */

  public Expression maybeInline (ApplyExp exp, Type required, Procedure proc)
  {
    try
      {
        Object inliner;
        synchronized (proc)
          {
            inliner = proc.getProperty(Procedure.validateApplyKey, null);
            if (inliner instanceof String)
              {
                String inliners = (String) inliner;
                int colon = inliners.indexOf(':');
                /* #ifdef use:java.dyn */
                // MethodHandle method = null;
                /* #else */
                java.lang.reflect.Method method = null;
                /* #endif */
                if (colon > 0)
                  {
                    String cname = inliners.substring(0, colon);
                    String mname = inliners.substring(colon+1);
                    Class clas = Class.forName(cname, true, proc.getClass().getClassLoader());
                    /* #ifdef use:java.dyn */
                    // method = MethodHandles.lookup().findStatic(clas, mname, validateApplyMType);
                    /* #else */
                    method = clas.getDeclaredMethod(mname, getInlinerMethodArgTypes());
                    /* #endif */
                  }
                if (method == null)
                  {
                    error('e', "inliner property string for "+proc+" is not of the form CLASS:METHOD");
                    return null;
                  }
                inliner = method;
              }
          } /* end synchronized */
        if (inliner != null)
          {
            /* #ifdef use:java.dyn */
            // if (inliner instanceof MethodHandle)
            //   return ((MethodHandle) inliner).<Expression>invokeExact(exp, this, required, proc);
            /* #endif */
            Object[] vargs = new Object[] { exp, this, required, proc };
            if (inliner instanceof Procedure)
              return (Expression) ((Procedure) inliner).applyN(vargs);
            /* #ifndef use:java.dyn */
            else if (inliner instanceof java.lang.reflect.Method)
              return (Expression) ((java.lang.reflect.Method) inliner)
                .invoke(null, vargs);
            /* #endif */
          }
      }
    catch (Throwable ex)
      {
        if (ex instanceof InvocationTargetException)
          ex = ((InvocationTargetException) ex).getTargetException();
        messages.error('e', "caught exception in inliner for "+proc+" - "+ex, ex);
      }
    return null;
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
