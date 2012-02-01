// Copyright (c) 2010, 2011  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;
import gnu.kawa.util.IdentityHashTable;
import gnu.mapping.*;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.BitOps;
import gnu.text.Char;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.HashMap;
import java.lang.reflect.Proxy;
import java.lang.annotation.ElementType;
/* #ifdef use:java.lang.invoke */
// import java.lang.invoke.*;
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

public class InlineCalls extends ExpExpVisitor<Type> {

    public static Expression inlineCalls (Expression exp, Compilation comp) {
        InlineCalls visitor = new InlineCalls(comp);
        return visitor.visit(exp, null);
    }

    public InlineCalls (Compilation comp) {
        setContext(comp);
    }

    VarValueTracker valueTracker = new VarValueTracker(this);

    public Expression visit (Expression exp, Type required) {
        if (! exp.getFlag(Expression.VALIDATED)) {
            exp.setFlag(Expression.VALIDATED); // Protect against cycles.
            exp = super.visit(exp, required);
            exp.setFlag(Expression.VALIDATED);
        }
        return checkType(exp, required);
    }

    public Expression checkType(Expression exp, Type required) {
        Type expType = exp.getType();
        if (expType == Type.toStringType)
            expType = Type.javalangStringType;
        int cmp = required == null ? 1 : required.compare(expType);
        boolean incompatible = cmp == -3;
        if (incompatible
            || (cmp == -2 && required.isInterface()
                && (exp instanceof QuoteExp || exp instanceof LambdaExp))) {
            if (exp instanceof LambdaExp
                && (required instanceof ClassType
                    || required instanceof ParameterizedType)) {
                ClassType reqraw = required instanceof ParameterizedType ? ((ParameterizedType) required).getRawType() : (ClassType) required;
                Method amethod = reqraw.checkSingleAbstractMethod();
                if (amethod != null) {
                    if (! ModuleExp.compilerAvailable()) {
                        if (! reqraw.isInterface())
                            comp.error('e', "cannot convert procedure to abstract class "+reqraw.getClass().getName()+" without bytecode compiler");
                        Class iface;
                        try {
                            iface = reqraw.getReflectClass();
                        }
                        catch (Throwable ex) {
                            iface = null;
                        }
                        if (iface == null)
                            comp.error('e', "cannot find interface "+reqraw.getClass().getName());
                        Method makeProxy =
                            ClassType.make("gnu.kawa.reflect.ProceduralProxy")
                            .getDeclaredMethod("makeProxy", 2);
                        Expression[] args = {QuoteExp.getInstance(iface), exp};
                        return visit(new ApplyExp(makeProxy, args), required);
                    }
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
            if (required instanceof TypeValue) {
                Expression converted = ((TypeValue) required).convertValue(exp);
                if (converted != null)
                    return converted;
            }

            Language language = comp.getLanguage();
            comp.error(processingAnnotations() ? 'e' : 'w',
                       ("type "+language.formatType(expType)
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
        Type primRequired;
        if (! exp.isExplicitlyTyped()
            && (primRequired = PrimType.unboxedType(required)) != null)
          {
            char sig1 = primRequired.getSignature().charAt(0);
            if (value instanceof IntNum)
              {
                IntNum ivalue = (IntNum) value;
                Object ival = null;
                switch (sig1)
                  {
                  case 'B':
                    if (ivalue.inRange(Byte.MIN_VALUE, Byte.MAX_VALUE))
                      ival = Byte.valueOf(ivalue.byteValue());
                    break;
                  case 'S':
                    if (ivalue.inRange(Short.MIN_VALUE, Short.MAX_VALUE))
                      ival = Short.valueOf(ivalue.shortValue());
                    break;
                  case 'I':
                    if (ivalue.inRange(Integer.MIN_VALUE, Integer.MAX_VALUE))
                      ival = Integer.valueOf(ivalue.intValue());
                    break;
                  case 'J':
                    if (ivalue.inRange(Long.MIN_VALUE, Long.MAX_VALUE))
                      ival = Long.valueOf(ivalue.longValue());
                    break;
                  case 'F':
                    ival = Float.valueOf(ivalue.floatValue());
                    break;
                  case 'D':
                    ival = Double.valueOf(ivalue.doubleValue());
                    break;
                  default:
                    ivalue = null;
                  }
                if (ival != null)
                  exp = new QuoteExp(ival, required);
                else if (ivalue != null)
                  error('w', "integer "+ivalue+" not in range of "+required.getName());
              }
            if (value instanceof DFloNum)
            {
              DFloNum dvalue = (DFloNum) value;
              Object dval;
              switch (sig1)
              {
                case 'F':
                  dval = Float.valueOf(dvalue.floatValue());
                  break;
                case 'D':
                  dval = Double.valueOf(dvalue.doubleValue());
                  break;
                default:
                  dval = null;
              }
              if (dval != null)
                exp = new QuoteExp(dval, required);
              else
                error('w', "saw float where "+required.getName()+" expected");
            }
            if (value instanceof Char && sig1 == 'C')
              {
                int ival = ((Char) value).intValue();
                if (ival >= 0 && ival <= 0xFFFF)
                  exp = new QuoteExp(Character.valueOf((char) ival), required);
              }
          }
        else if ((value instanceof IntNum) &&
                 !exp.isExplicitlyTyped() && required != null &&
                 "java.math.BigInteger".equals(required.getName()))
        {
          exp = new QuoteExp(((IntNum)value).asBigInteger(), required);
        }
      }
    return exp;
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Type required)
  {
    Declaration decl = exp.getBinding();
    if (decl != null)
      {
        IntNum vals = valueTracker.declValueUsage.get(decl);
        if (vals != null)
          {
            if (VarValueTracker.maybeUninitialized(vals)
                && ! decl.getFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS))
              {
                comp.error('w', "variable '"+exp.getName()+"' may be uninitialized here", exp);
                decl.setFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS);
              }
          }
      }
    if (decl != null && decl.field == null && ! decl.getCanWrite())
      {
        LambdaExp lval = decl.getLambdaValue();
        if (lval != null)
          valueTracker.checkUninitializedVariables(lval, exp, null);
        Expression dval = decl.getValue();
        if (dval instanceof LambdaExp && ! exp.getDontDereference() && ! (dval instanceof ClassExp) && ! dval.getFlag(Expression.VALIDATED))
          {
            visit(dval, null);
          }
        if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
          return visitQuoteExp((QuoteExp) dval, required);
        // We don't want to visit the body of a named function yet.
        if (decl.nvalues == 1 && decl.values[0].kind == Declaration.ValueSource.APPLY_KIND)
          dval = null;
        if (dval instanceof ReferenceExp && ! decl.isAlias())
          {
            ReferenceExp rval = (ReferenceExp) dval;
            Declaration rdecl = rval.getBinding();
            Type dtype = decl.getType();
            if (rdecl != null && ! rdecl.getCanWrite()
                && (dtype == null || dtype == Type.objectType
                    // We could also allow (some) widening conversions.
                    || dtype == rdecl.getType())
                && ! rval.getDontDereference())
              return visitReferenceExp(rval, required);
          }
        if (dval instanceof ClassExp && processingAnnotations())
          {
            ClassExp cval = (ClassExp) dval;
            if (cval.compiledType != null)
              return new QuoteExp(cval.compiledType, required);
          }
        if (! exp.isProcedureName() && decl.isClassMethod())
          {
            // FIXME.  This shouldn't be that hard to fix.  For example,
            // we could treat a reference to a one-argument method foo as if it
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
    VarValueTracker.forkPush(this);
    if (exitValue == null)
      exp.then_clause = visit(exp.then_clause, required);
    valueTracker.forkNext();
    if (exitValue == null && exp.else_clause != null)
      exp.else_clause = visit(exp.else_clause, required);
    VarValueTracker.forkPop(this);
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
        visitAnnotations(decl);
      }
    return exp;
  }

  /** Visit any named functions that haven't been visit yet.
   * This should be called at the end of a LetExp or ModuleExp.
   */
  protected void visitRemainingDeclaredLambdas (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl(); decl != null;
         decl = decl.nextDecl())
      {
        Expression value = decl.getValueRaw();
        if (value instanceof LambdaExp)
          visit(value, null);
      }
  }

  protected Expression visitModuleExp (ModuleExp exp, Type required)
  {
    LambdaExp saveLambda = currentLambda;
    currentLambda = exp;
    try
      {
        super.visitModuleExp(exp, required);
      }
    finally
      {
        currentLambda = saveLambda;
      }
    visitRemainingDeclaredLambdas(exp);
    return exp;
  }

  protected Expression visitLetExp (LetExp exp, Type required)
  {
    if (! (exp instanceof CatchClause) && ! (exp instanceof FluidLetExp))
      valueTracker.noteUnitialized(exp);

    for (Declaration decl = exp.firstDecl(); decl != null; decl = decl.nextDecl())
      {
	Expression init = decl.getInitValue();
        if (decl.nvalues > 0
            && decl.values[0].kind == Declaration.ValueSource.LET_INIT_KIND
            && decl.values[0].base == exp)
          {
            valueTracker.noteSet(decl, IntNum.make(~0));
          }
        boolean typeSpecified = decl.getFlag(Declaration.TYPE_SPECIFIED);
        Type dtype = typeSpecified && init != QuoteExp.undefined_exp ? decl.getType() : null;
        if (init instanceof LambdaExp && ! (init instanceof ClassExp) && decl.getValueRaw() == init)
          ; // defer
        else
          init = visit(init, dtype);
	decl.setInitValue(init);
      }

    if (exitValue == null)
      exp.body = visit(exp.body, required);
    if (exp.body instanceof ReferenceExp)
      {
        ReferenceExp ref = (ReferenceExp) exp.body;
        Declaration d = ref.getBinding();
        if (d != null && d.context == exp && ! ref.getDontDereference())
          {
            if (exp.firstDecl() == d && d.nextDecl() == null) // Single decl
              {
                Expression init = d.getInitValue();
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
    visitRemainingDeclaredLambdas(exp);
    return exp;
  }

  protected Expression visitLambdaExp (LambdaExp exp, Type required)
  {
    if (exp.getCallConvention() == Compilation.CALL_WITH_UNSPECIFIED)
      exp.setCallConvention(getCompilation());
    Declaration firstDecl = exp.firstDecl();
    if (firstDecl != null && firstDecl.isThisParameter()
        && ! exp.isClassMethod() && firstDecl.type == null)
      {
        firstDecl.setType(comp.mainClass);
      }
    if (exp.getClass() == LambdaExp.class)
      {
        Declaration ldecl = exp.nameDecl;
        boolean unknownCalls = true;
        if (ldecl != null && ! exp.isClassMethod()
            && (ldecl.isPrivate() || ! (ldecl.context instanceof ModuleExp)))
          {
            int countApply = 0;
            for (ApplyExp app = ldecl.firstCall; app != null;
                 app = app.nextCall)
              countApply++;
            if (countApply == ldecl.numReferences)
              {
                // Some preliminary data-flow from a set of known call sites.
                // This isn't fully implemented yet.
                unknownCalls = false;
                for (ApplyExp app = ldecl.firstCall; app != null;
                     app = app.nextCall)
                  {
                    Expression func = app.getFunction();
                    int nargs = app.getArgCount();
                    Declaration p = firstDecl;
                    if (p != null && p.isThisParameter())
                        p = p.nextDecl();
                    int i = 0;
                    for (; p != null && i < exp.min_args; p = p.nextDecl(), i++)
                      {
                        if (! p.hasUnknownValue())
                          p.noteValueFromApply(app, i);
                      }
                  }
              }
          }
        if (unknownCalls)
          {
            for (Declaration p = firstDecl; p != null;  p = p.nextDecl())
              {
                if (! p.isThisParameter())
                  p.noteValueUnknown();
              }
          }
      }
    LambdaExp saveLambda = currentLambda;
    currentLambda = exp;
    try
      {
        visitScopeExp(exp, required);
      }
    finally
      {
        currentLambda = saveLambda;
      }
    if (exp.isClassMethod() && "*init*".equals(exp.getName()))
      {
        Expression bodyFirst = exp.getBodyFirstExpression();
        ClassType calledInit = exp.checkForInitCall(bodyFirst);
        ClassExp cexp = (ClassExp) exp.outer;
        ClassType superClass = cexp.instanceType.getSuperclass();
        if (calledInit != null)
          {
            if (calledInit != cexp.instanceType && calledInit != superClass)
              comp.error('e', "call to <init> for not this or super class");
          }
        else if (superClass != null)
          {
            cexp.checkDefaultSuperConstructor(superClass, comp);
          }
      }
    return exp;
  }

  public void visitDefaultArgs (LambdaExp exp, Type required)
  {
    for (Declaration p = exp.firstDecl(); p != null; p = p.nextDecl())
      {
        Expression init = p.getInitValue();
        if (init != null)
          p.setInitValue(visitAndUpdate(init, p.getType()));
      }
  } 

  protected Expression visitClassExp (ClassExp exp, Type required) {
      Expression result = super.visitClassExp(exp, required);
      if (! exp.explicitInit && ! exp.instanceType.isInterface())
          exp.checkDefaultSuperConstructor(exp.instanceType.getSuperclass(), comp);
      return result;
  }

  protected Expression visitTryExp (TryExp exp, Type required)
  {
    if (exp.getCatchClauses() == null && exp.getFinallyClause() == null)
      return visit(exp.try_clause, required);
    else
      return super.visitTryExp(exp, required);
  }

  boolean processingAnnotations;
  /** If currently processing an annotation belonging to a declaration.
   * In this case expressions must resolve to constants,
   * annotations must resolve to know annotation types.
   */
  public boolean processingAnnotations () { return processingAnnotations; }

  protected void visitAnnotations (Declaration decl)
  {
    List<Expression> annotations = decl.annotations;
    if (annotations != null)
      {
        boolean saveProcessingAnnotations = processingAnnotations;
        processingAnnotations = true;
        try
          {
            int num = annotations.size();
            for (int i = 0;  i < num;  i++)
              {
                Expression before = annotations.get(i);
                Expression ann = visit(before, null);
                Object aval = ann.valueIfConstant();
                if (aval instanceof Proxy
                    && ((aval = Proxy.getInvocationHandler(aval))
                        instanceof AnnotationEntry))
                  {
                    AnnotationEntry ae = (AnnotationEntry) aval;
                    if (decl.isClassMethod() && !ae.hasTarget(ElementType.METHOD))
                      comp.error('e', "annotation "+ae.getAnnotationType().getName()+" allowed on methods", before);
                    if (decl.isClassField() && !ae.hasTarget(ElementType.FIELD))
                      comp.error('e', "annotation "+ae.getAnnotationType().getName()+" not allowed on fields", before);
                    if (decl.getValue() instanceof ClassExp
                        && !ae.hasTarget(ElementType.TYPE)
                        && !ae.hasTarget(ElementType.FIELD))
                      comp.error('e', "annotation "+ae.getAnnotationType().getName()+" not allowed on classes", before);
                  }
                annotations.set(i, ann);
              }
          }
        finally
          {
            processingAnnotations = saveProcessingAnnotations;
          }
      }
  }

  protected Expression visitSetExp (SetExp exp, Type required)
  {
    Declaration decl = exp.getBinding();
    if (decl != null && decl.values != Declaration.unknownValueValues
        && exp.valueIndex >= 0)
      {
        IntNum setterMask = IntNum.make(~exp.valueIndex);
        valueTracker.noteSet(decl, setterMask);
      }
    if (decl != null && decl.getLambdaValue() != null)
      ; // defer
    else 
      exp.new_value = visit(exp.new_value, decl == null || decl.isAlias() ? null : decl.type);
    if (! exp.isDefining() && decl != null && decl.isClassMethod())
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

  /* #ifdef use:java.lang.invoke */
  // static final MethodType validateApplyMType =
  //   MethodType.methodType(gnu.expr.Expression.class,
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
                /* #ifdef use:java.lang.invoke */
                // MethodHandle method = null;
                /* #else */
                java.lang.reflect.Method method = null;
                /* #endif */
                if (colon > 0)
                  {
                    String cname = inliners.substring(0, colon);
                    String mname = inliners.substring(colon+1);
                    Class clas = Class.forName(cname, true, proc.getClass().getClassLoader());
                    /* #ifdef use:java.lang.invoke */
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
            /* #ifdef use:java.lang.invoke */
            // if (inliner instanceof MethodHandle)
            //   return (Expression) ((MethodHandle) inliner).invokeExact(exp, this, required, proc);
            /* #endif */
            Object[] vargs = new Object[] { exp, this, required, proc };
            if (inliner instanceof Procedure)
              return (Expression) ((Procedure) inliner).applyN(vargs);
            /* #ifndef use:java.lang.invoke */
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
        LetExp let = new LetExp();
        for (Declaration param = lexp.firstDecl(); param != null; i++)
          {
            Declaration next = param.nextDecl();
            param.setInitValue(cargs[i]);
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
                if ( ! param.getCanWrite()) {
                  param.nvalues = 0;
                  param.values = null;
                  param.noteValueFromLet(let);
                }
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
