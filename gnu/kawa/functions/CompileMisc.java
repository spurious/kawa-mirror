package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.lispexpr.LangObjType;
import gnu.lists.LList;
import kawa.standard.Scheme;

public class CompileMisc implements Inlineable
{
  static final int CONVERT = 2;
  static final int NOT = 3;
  int code;
  Procedure proc;

  public CompileMisc(Procedure proc, int code)
  {
    this.proc = proc;
    this.code = code;
  }

  public static CompileMisc forConvert(Object proc)
  {
    return new CompileMisc((Procedure) proc, CONVERT);
  }

  public static CompileMisc forNot(Object proc)
  {
    return new CompileMisc((Procedure) proc, NOT);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    switch (code)
      {
      case CONVERT:
        compileConvert((Convert) proc, exp, comp, target);
        return;
      case NOT:
        compileNot((Not) proc, exp, comp, target);
        return;
      default: throw new Error();
      }
  }

  public static Expression validateApplyConstantFunction0
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    int nargs = exp.getArgCount();
    if (nargs != 0 && visitor != null)
      {
	String message = WrongArguments.checkArgCount(proc, nargs);
	return visitor.noteError(message);
      }
    return ((ConstantFunction0) proc).constant;
  }

  public static Expression validateApplyConvert
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Compilation comp = visitor.getCompilation();
    Language language = comp.getLanguage();
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        args[0] = visitor.visit(args[0], null);
        Type type = language.getTypeFor(args[0]);
        if (type instanceof Type)
          {
            args[0] = new QuoteExp(type);
            args[1] = visitor.visit(args[1], type);
            CompileReflect.checkKnownClass(type, comp);
            exp.setType(type);
            return exp;
          }
      }
    exp.visitArgs(visitor);
    return exp;
  }

  public static Expression validateApplyNot
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(visitor.getCompilation().getLanguage().getTypeFor(Boolean.TYPE));
    return exp.inlineIfConstant(proc, visitor);
  }

  /** Validate-apply handling for "format".
   * Sets the correct return-type, and may replace by call to a static method.
   */
  public static Expression validateApplyFormat
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Type retType = Type.objectType;
    Expression[] args = exp.getArgs();
    if (args.length > 0)
      {
        ClassType typeFormat = ClassType.make("gnu.kawa.functions.Format");
        Object f = args[0].valueIfConstant();
        Type ftype = args[0].getType();
        if (f == Boolean.FALSE || ftype.isSubtype(LangObjType.stringType))
          {
            int skip = f == Boolean.FALSE ? 1 : 0;
            Expression[] xargs = new Expression[args.length+1-skip];
            xargs[0] = new QuoteExp(Integer.valueOf(0), Type.intType);
            System.arraycopy(args, skip, xargs, 1, xargs.length-1);
            ApplyExp ae = new ApplyExp(typeFormat.getDeclaredMethod("formatToString", 2), xargs);
            ae.setType(Type.javalangStringType);
            return ae;
          }
        if (f == Boolean.TRUE
            || ftype.isSubtype(ClassType.make("java.io.Writer")))
          {
            if (f == Boolean.TRUE)
              {
                Expression[] xargs = new Expression[args.length];
                xargs[0] = QuoteExp.nullExp;
                System.arraycopy(args, 1, xargs, 1, args.length-1);
                args = xargs;
              }
            ApplyExp ae = new ApplyExp(typeFormat.getDeclaredMethod("formatToWriter", 3), args);
            ae.setType(Type.voidType);
            return ae;
          }
        if (ftype.isSubtype(ClassType.make("java.io.OutputStream")))
          retType = Type.voidType;
      }
    exp.setType(retType);
    return null;
  }

  public static Expression validateApplyAppendValues
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      return args[0];
    if (args.length == 0)
      return QuoteExp.voidExp;
    Expression folded = exp.inlineIfConstant(proc, visitor);
    if (folded != exp)
      return folded;
    return exp;
  }

  public static Expression validateApplyMakeProcedure
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    int alen = args.length;
    Expression method = null;
    int countMethods = 0;
    String name = null;
    for (int i = 0;  i < alen;  i++)
      {
	Expression arg = args[i];
        Object key;
	if (arg instanceof QuoteExp
            && (key = ((QuoteExp) arg).getValue()) instanceof Keyword)
	  {
	    String keyword = ((Keyword) key).getName();
	    Expression next = args[++i];
	    if (keyword == "name")
              {
                if (next instanceof QuoteExp)
                  name = ((QuoteExp) next).getValue().toString();
              }
	    else if (keyword == "method")
              {
                countMethods++;
                method = next;
              }
	    else
	      ; // result.setProperty(keyword, value);
	  }
	else
          {
            countMethods++;
            method = arg;
          }
      }
    if (countMethods == 1 && method instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) method;
        for (int i = 0;  i < alen;  i++)
          {
            Expression arg = args[i];
            Object key;
            if (arg instanceof QuoteExp
                && (key = ((QuoteExp) arg).getValue()) instanceof Keyword)
              {
                String keyword = ((Keyword) key).getName();
                Expression next = args[++i];
                if (keyword == "name")
                  lexp.setName(name);
                else if (keyword == "method")
                  ;
                else
                  lexp.setProperty(Namespace.EmptyNamespace.getSymbol(keyword), next);
              }
          }
        return method;
      }
    return exp;
  }

  public static Expression validateApplyValuesMap
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    LambdaExp lexp = ValuesMap.canInline(exp, (ValuesMap) proc);
    if (lexp != null)
      {
	lexp.setInlineOnly(true);
        lexp.returnContinuation = exp;
        lexp.inlineHome = visitor.getCurrentLambda();
      }
    return exp;
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

  public static void compileConvert (Convert proc, ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      throw new Error ("wrong number of arguments to "+proc.getName());
    CodeAttr code = comp.getCode();
    Type type = Scheme.getTypeValue(args[0]);
    if (type != null)
      {
        args[1].compile(comp, Target.pushValue(type));
	if (code.reachableHere())
	  target.compileFromStack(comp, type);
      }
    else
      {
	if (typeType == null)
	  {
	    typeType = ClassType.make("gnu.bytecode.Type");
          }
        if (coerceMethod == null)
          {
	    coerceMethod = typeType.addMethod("coerceFromObject",
					      Compilation.apply1args,
					      Type.pointer_type,
					      gnu.bytecode.Access.PUBLIC);
	  }
	args[0].compile(comp, LangObjType.typeClassType);
	args[1].compile(comp, Target.pushObject);
	code.emitInvokeVirtual(coerceMethod);
	target.compileFromStack(comp, Type.pointer_type);
      }
  }

  public void compileNot (Not proc, ApplyExp exp, Compilation comp, Target target)
  {
    Expression arg = exp.getArgs()[0];
    Language language = proc.language;
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarget = (ConditionalTarget) target;
	ConditionalTarget sub_target
	  = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue, language);
	sub_target.trueBranchComesFirst = ! ctarget.trueBranchComesFirst;
	arg.compile(comp, sub_target);
	return;
      }
    CodeAttr code = comp.getCode();
    Type type = target.getType();
    if (target instanceof StackTarget && type.getSignature().charAt(0) == 'Z')
      {
	arg.compile(comp, target);
	code.emitNot(target.getType());
      }
    else
      {
        QuoteExp trueExp = QuoteExp.getInstance(language.booleanObject(true));
        QuoteExp falseExp = QuoteExp.getInstance(language.booleanObject(false));
	IfExp.compile(arg, falseExp, trueExp, comp, target);
      }
  }

  public static Expression validateApplyCallCC
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    LambdaExp lexp = canInlineCallCC(exp);
    if (lexp != null)
      {
	lexp.setInlineOnly(true);
	lexp.returnContinuation = exp;
        lexp.inlineHome = visitor.getCurrentLambda();
        Declaration contDecl = lexp.firstDecl();
        if (! contDecl.getFlag(Declaration.TYPE_SPECIFIED))
          contDecl.setType(typeContinuation);
      }
    exp.visitArgs(visitor);
    return exp;
  }

  public static final ClassType typeContinuation =
    ClassType.make("kawa.lang.Continuation");

  public static void compileCallCC (ApplyExp exp, Compilation comp, Target target, Procedure proc)
  {
    LambdaExp lambda = canInlineCallCC(exp);
    if (lambda == null)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    CodeAttr code = comp.getCode();
    final Declaration param = lambda.firstDecl();
    if (param.isSimple() && ! param.getCanRead() && ! param.getCanWrite())
      {
        CompileTimeContinuation contProxy = new CompileTimeContinuation();
        Type rtype = target instanceof StackTarget ? target.getType() : null;
        boolean runFinallyBlocks
          = ExitThroughFinallyChecker.check(param, lambda.body);
        ExitableBlock bl = code.startExitableBlock(rtype, runFinallyBlocks);
        contProxy.exitableBlock = bl;
        contProxy.blockTarget = target;
        param.setValue(new QuoteExp(contProxy));
        lambda.body.compile(comp, target);
        code.endExitableBlock();
        return;
      }

    Scope sc = code.pushScope();
    Variable contVar = sc.addVariable(code, typeContinuation, null);
    Declaration contDecl = new Declaration(contVar);
    code.emitNew(typeContinuation);
    code.emitDup(typeContinuation);
    comp.loadCallContext();
    code.emitInvokeSpecial(typeContinuation.getDeclaredMethod("<init>", 1));
    code.emitStore(contVar);
    code.emitTryStart(false, target instanceof IgnoreTarget || target instanceof ConsumerTarget ? null : Type.objectType);
    ApplyExp app = new ApplyExp(lambda, new Expression[] { new ReferenceExp(contDecl) });
    app.compile(comp, target);
    // Emit: cont.invoked = true
    if (code.reachableHere())
      {
        code.emitLoad(contVar);
        code.emitPushInt(1);
        code.emitPutField(typeContinuation.getField("invoked"));
      }
    code.emitTryEnd();

    // Emit: catch (Throwable ex) { handleException$(ex, cont, ctx); }
    code.emitCatchStart(null);
    code.emitLoad(contVar);
    if (target instanceof ConsumerTarget)
      {
        comp.loadCallContext();
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException$X", 3);
        code.emitInvokeStatic(handleMethod);
      }
    else
      {
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException", 2);
        code.emitInvokeStatic(handleMethod);
        target.compileFromStack(comp, Type.objectType);
      }
    code.emitCatchEnd();

    code.emitTryCatchEnd();
    code.popScope();
  }

  /** If we can inline, return LambdaExp for first arg; otherwise null. */
  private static LambdaExp canInlineCallCC (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    Expression arg0;
    if (args.length == 1 && (arg0 = args[0]) instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) arg0;
        if (lexp.min_args == 1 && lexp.max_args == 1
            && ! lexp.firstDecl().getCanWrite())
          {
            return lexp;
          }
      }
    return null;
  }

  /** An ExpVisitor class to check if callcc exits through a try-finally. */
  static class ExitThroughFinallyChecker extends ExpVisitor<Expression,TryExp>
  {
    Declaration decl;

    /** Does decl appear in body nested inside a try-finally? */
    public static boolean check (Declaration decl, Expression body)
    {
      ExitThroughFinallyChecker visitor = new ExitThroughFinallyChecker();
      visitor.decl = decl;
      visitor.visit(body, null);
      return visitor.exitValue != null;
    }

    protected Expression defaultValue(Expression r, TryExp d)
    {
      return r;
    }

    protected Expression visitReferenceExp (ReferenceExp exp, TryExp currentTry)
    {
      if (decl == exp.getBinding() && currentTry != null)
        exitValue = Boolean.TRUE;
      return exp;
    }

    protected Expression visitTryExp (TryExp exp, TryExp currentTry)
    {
      visitExpression(exp, exp.getFinallyClause() != null ? exp : currentTry);
      return exp;
    }
  }

  public static Expression validateApplyMap
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure xproc)
  {
    Map mproc = (Map) xproc;
    boolean collect = mproc.collect;
    // FIXME: We should inline the list arguments first before inlining the
    // procedure argument, for better type inference etc.
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs < 2)
      return exp;  // ERROR

    nargs--;

    Expression proc = args[0];
    // If evaluating proc doesn't have side-effects, then we want to do
    // so inside loop, since that turns a "read" info a "call", which
    // may allow better inlining.
    boolean procSafeForMultipleEvaluation = ! proc.side_effects();

    // First an outer (let ((%proc PROC)) L2), where PROC is args[0].
    Expression[] inits1 = new Expression[1];
    inits1[0] = proc;
    LetExp let1 = new LetExp(inits1);
    Declaration procDecl
      = let1.addDeclaration("%proc", Compilation.typeProcedure);
    procDecl.noteValue(args[0]);

    // Then an inner L2=(let ((%loop (lambda (argi ...) ...))) (%loop ...))
    Expression[] inits2 = new Expression[1];
    LetExp let2 = new LetExp(inits2);
    let1.setBody(let2);
    LambdaExp lexp = new LambdaExp(collect ? nargs + 1 : nargs);
    inits2[0] = lexp;
    Declaration loopDecl = let2.addDeclaration("%loop");
    loopDecl.noteValue(lexp);

    // Finally an inner L3=(let ((parg1 (as <pair> arg1)) ...) ...)
    Expression[] inits3 = new Expression[nargs];
    LetExp let3 = new LetExp(inits3);

    Declaration[] largs = new Declaration[nargs];
    Declaration[] pargs = new Declaration[nargs];
    for (int i = 0;  i < nargs;  i++)
      {
	String argName = "arg"+i;
	largs[i] = lexp.addDeclaration(argName);
	pargs[i] = let3.addDeclaration(argName, Compilation.typePair);
	inits3[i] = new ReferenceExp(largs[i]);
	pargs[i].noteValue(inits3[i]);
      }
    Declaration resultDecl = collect ? lexp.addDeclaration("result") : null;
    Expression[] doArgs = new Expression[1+nargs];
    Expression[] recArgs = new Expression[collect ? nargs + 1 : nargs];
    for (int i = 0;  i < nargs;  i++)
      {
	doArgs[i+1] = visitor.visitApplyOnly(SlotGet.makeGetField(new ReferenceExp(pargs[i]), "car"), null);
	recArgs[i] = visitor.visitApplyOnly(SlotGet.makeGetField(new ReferenceExp(pargs[i]), "cdr"), null);
      }
    if (! procSafeForMultipleEvaluation)
      proc = new ReferenceExp(procDecl);
    doArgs[0] = proc;
    Expression doit = visitor.visitApplyOnly(new ApplyExp(new ReferenceExp(mproc.applyFieldDecl), doArgs), null);
    if (collect)
      {
	Expression[] consArgs = new Expression[2];
	consArgs[0] = doit;
	consArgs[1] = new ReferenceExp(resultDecl);
	recArgs[nargs] = Invoke.makeInvokeStatic(Compilation.typePair,
						 "make", consArgs);
      }
    Expression rec = visitor.visitApplyOnly(new ApplyExp(new ReferenceExp(loopDecl), recArgs), null);
    lexp.body = collect ? rec : new BeginExp(doit, rec);
    let3.setBody(lexp.body);
    lexp.body = let3;
    Expression[] initArgs = new Expression[collect ? nargs + 1 : nargs];
    QuoteExp empty = new QuoteExp(LList.Empty);
    for (int i = nargs;  --i >= 0; )
      {
	Expression[] compArgs = new Expression[2];
	compArgs[0] = new ReferenceExp(largs[i]);
	compArgs[1] = empty;
	Expression result
	  = collect ? (Expression) new ReferenceExp(resultDecl)
	  : (Expression) QuoteExp.voidExp;
	lexp.body = new IfExp(visitor.visitApplyOnly(new ApplyExp(mproc.isEq, compArgs), null),
			      result, lexp.body);
	initArgs[i] = args[i+1];
      }
    if (collect)
      initArgs[nargs] = empty;

    Expression body = visitor.visitApplyOnly(new ApplyExp(new ReferenceExp(loopDecl), initArgs), null);
    if (collect)
      {
	Expression[] reverseArgs = new Expression[1];
	reverseArgs[0] = body;
	body = Invoke.makeInvokeStatic(Compilation.scmListType,
				       "reverseInPlace", reverseArgs);
      }
    let2.setBody(body);

    if (procSafeForMultipleEvaluation)
      return let2;
    else
      return let1;
  }
}
