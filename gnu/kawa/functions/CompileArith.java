package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangPrimType;

public class CompileArith implements CanInline, Inlineable
{
  static final int ADD = 1;
  static final int SUB = 2;
  static final int MUL = 3;
  int code;
  Procedure proc;

  CompileArith(Object proc, int code)
  {
    this.proc = (Procedure) proc;
    this.code = code;
  }

  public static CompileArith forAddSub(Object proc)
  {
    return new CompileArith(proc, ((AddOp) proc).plusOrMinus > 0 ? ADD : SUB);
  }

  public static CompileArith forMul(Object proc)
  {
    return new CompileArith(proc, MUL);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    switch (code)
      {
      case ADD:
      case SUB:
        return inlineAdd((AddOp) proc, exp, walker, argsInlined);
      case MUL:
        return inlineMul((MultiplyOp) proc, exp, walker, argsInlined);
      default: throw new Error();
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int len = args.length;
    if (len == 0)
      {
	comp.compileConstant(((ArithOp) proc).defaultResult(), target);
	return;
      }
    if (len == 1 || target instanceof IgnoreTarget)
      {
	// FIXME implement optimization for unary
	ApplyExp.compile(exp, comp, target);
	return;
      }
    // We know len >= 2 from above.
    // We expect len == 2, assuming inline has been run.
    int kind1 = Arithmetic.classifyType(args[0].getType());
    int kind2 = Arithmetic.classifyType(args[1].getType());
    int kind = getReturnKind(kind1, kind2);
    Type type = Arithmetic.kindType(kind);
    if (kind == 0 || len != 2 /* just in case */)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Type targetType = target.getType();
    int tkind = Arithmetic.classifyType(targetType);
    Type wtype;
    if ((tkind == Arithmetic.INT_CODE || tkind == Arithmetic.LONG_CODE)
        && kind >= Arithmetic.INT_CODE && kind <= Arithmetic.INTNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.INT_CODE ? LangPrimType.intType
          : LangPrimType.longType;
      }
    else if ((tkind == Arithmetic.DOUBLE_CODE
              || tkind == Arithmetic.FLOAT_CODE)
             
             && kind > 0 && kind <= Arithmetic.REALNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.FLOAT_CODE ? LangPrimType.floatType
          : LangPrimType.doubleType;

      }
    else if (kind == Arithmetic.FLOAT_CODE)
      wtype = LangPrimType.floatType;
    else if (kind == Arithmetic.DOUBLE_CODE || kind == Arithmetic.FLONUM_CODE)
      {
        kind = Arithmetic.DOUBLE_CODE;
        wtype = LangPrimType.doubleType;
      }
    else
      wtype = type;

    if (kind != Arithmetic.INT_CODE
        && kind != Arithmetic.LONG_CODE
        && kind != Arithmetic.FLOAT_CODE
        && kind != Arithmetic.DOUBLE_CODE)
      {
        // FIXME
        ApplyExp.compile(exp, comp, target);
        return;
      }
      
    Target wtarget = StackTarget.getInstance(wtype);

    CodeAttr code = comp.getCode();
    for (int i = 0;  i < len;  i++)
      {
        args[i].compile(comp, wtarget);
        if (i == 0)
          continue;
        switch (kind)
          {
          case Arithmetic.INT_CODE:
          case Arithmetic.LONG_CODE:
          case Arithmetic.FLOAT_CODE:
          case Arithmetic.DOUBLE_CODE:
            code.emitBinop(primitiveOpcode(), (PrimType) wtype.getImplementationType());
            break;
          }
      }
    target.compileFromStack(comp, wtype);
  }

  public int getReturnKind (int kind1, int kind2)
  {
    return kind1 > kind2 || kind1 == 0 ? kind1 : kind2;
  }

  public int getReturnKind (Expression[] args)
  {
    int len = args.length;
    if (len == 0)
      return Arithmetic.INTNUM_CODE;
    Type type = Type.pointer_type;
    int kindr = 0;
    for (int i = 0;  i < len;  i++)
      {
	Expression arg = args[i];
	int kind = Arithmetic.classifyType(arg.getType());

	if (i == 0 || kind == 0 || kind > kindr)
	  kindr = kind;
      }
    return kindr;
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    switch (code)
      {
      default:
        return Arithmetic.kindType(getReturnKind(args));
      }
  }

  public Expression inlineAdd (AddOp proc, ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    // Inlining may yield PrimProcedure instructions of bytecode instructions
    // which we don't know how to interpret (yet).
    if (! walker.getCompilation().mustCompile)
      return exp;
    Expression folded = exp.inlineIfConstant(proc, walker);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(proc, exp.getFunction(), args, walker);
    if (args.length == 1 && proc.plusOrMinus < 0)
      {
        Type type0 = args[0].getType();
        if (type0 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            Type type = null;
            int opcode = 0;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C')
              {
                // error
              }
            else if (sig0 == 'D')
              {
                opcode = 119 /* dneg */;
                type = LangPrimType.doubleType;
              }
            else if (sig0 == 'F')
              {
                opcode = 118 /* fneg */;
                type = LangPrimType.floatType;
              }
            else if (sig0 == 'J')
              {
                opcode = 117 /* lneg */;
                type = LangPrimType.longType;
              }
            else
              {
                opcode = 116 /* ineg */;
                type = LangPrimType.intType;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinUnary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    if (args.length == 2)
      {
	return primInline(primitiveOpcode(), exp);
      }
    return exp;
  }

  public static Expression inlineMul (MultiplyOp proc,
                                      ApplyExp exp, InlineCalls walker,
                                      boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
    if (! walker.getCompilation().mustCompile)
      return exp;
    Expression folded = exp.inlineIfConstant(proc, walker);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(proc, exp.getFunction(), args, walker);
    if (args.length == 2)
      return primInline(104, exp);
		
    return exp;
  }

  public static Expression primInline (int opcode, ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        Type type0 = args[0].getType();
        Type type1 = args[1].getType();
        if (type0 instanceof PrimType && type1 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            char sig1 = type1.getSignature().charAt(0);
            Type type = null;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C'
                || sig1 == 'V' || sig1 == 'Z' || sig1 == 'C')
              {
                // error
              }
            else if (sig0 == 'D' || sig1 == 'D')
              {
                opcode += 3;
                type = LangPrimType.doubleType;
              }
            else if (sig0 == 'F' || sig1 == 'F')
              {
                opcode += 2;
                type = LangPrimType.floatType;
              }
            else if (sig0 == 'J' || sig1 == 'J')
              {
                opcode += 1;
                type = LangPrimType.longType;
              }
            else
              {
                type = LangPrimType.intType;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinBinary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    return exp;
  }

  public int primitiveOpcode ()
  {
    switch (code)
      {
      case ADD:    return 96; /* iadd */
      case SUB:    return 100; /* isub */
      case MUL:    return 104;
      default:     return -1;
      }
  }

  /** Convert (PROC A B C) to (PROC (PROC A B) C) etc.
   */
  public static Expression pairwise(Procedure proc,
                                    Expression rproc, Expression[] args,
				    InlineCalls walker)
  {
    int len = args.length;
    Expression prev = args[0];
    for (int i = 1;  i < len;  i++)
      {
        Expression[] args2 = new Expression[2];
        args2[0] = prev;
        args2[1] = args[i];
        ApplyExp next = new ApplyExp(rproc, args2);
        Expression inlined = walker.maybeInline(next, true, proc);
        prev = inlined != null ? inlined : next;
      }
    return prev;
  }
}
