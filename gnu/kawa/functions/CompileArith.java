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
  int op;
  Procedure proc;

  public static CompileArith $Pl = new CompileArith(AddOp.$Pl, ADD);
  public static CompileArith $Mn = new CompileArith(AddOp.$Mn, SUB);

  CompileArith(Object proc, int op)
  {
    this.proc = (Procedure) proc;
    this.op = op;
  }

  public static CompileArith forMul(Object proc)
  {
    return new CompileArith(proc, MUL);
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    switch (op)
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

    if (kind == Arithmetic.INTNUM_CODE
        && (op == ADD || op == MUL || op == SUB))
      {
        compileIntNum(args[0], args[1], kind1, kind2, comp);
      }
      else if (kind != Arithmetic.INT_CODE
        && kind != Arithmetic.LONG_CODE
        && kind != Arithmetic.FLOAT_CODE
        && kind != Arithmetic.DOUBLE_CODE)
      {
        // FIXME
        ApplyExp.compile(exp, comp, target);
        return;
      }
    else
      {
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
      }
    target.compileFromStack(comp, wtype);
  }

  static boolean inRange (Expression exp, int lo, int hi)
  {
    Object val = exp.valueIfConstant();
    return val instanceof IntNum && inRange((IntNum) val, lo, hi);
  }

  static boolean inRange (IntNum val, int lo, int hi)
  {
    return IntNum.compare(val, Integer.MIN_VALUE) >= 0
      && IntNum.compare(val, Integer.MAX_VALUE) <= 0;
  }

  public boolean compileIntNum (Expression arg1, Expression arg2, int kind1, int kind2, Compilation comp)
  {
    // Check if we can replace ARG1-CONSTANT by ARG1+(-CONSTANT),
    // where (-CONSTANT) is an int, so we can use IntNum.add(IntNum,int).
    if (op == SUB && arg2 instanceof QuoteExp)
      {
        Object val = arg2.valueIfConstant();
        long lval;
        boolean negateOk;
        if (kind2 <= Arithmetic.LONG_CODE)
          {
            lval = ((Number) val).longValue();
            negateOk = lval > Integer.MIN_VALUE && lval <= Integer.MAX_VALUE;
          }
        else if (val instanceof IntNum)
          {
            IntNum ival = (IntNum) val;
            lval =  ival.longValue();
            negateOk = inRange(ival, Integer.MIN_VALUE+1, Integer.MAX_VALUE);
          }
        else
          {
            negateOk = false;
            lval = 0;
          }
        if (negateOk)
          return $Pl.compileIntNum(arg1,
                                   QuoteExp.getInstance(Integer.valueOf((int) - lval)),
                                   kind1, Arithmetic.INT_CODE, comp);
      }
    boolean swap;
    boolean addOrMul = op == ADD || op == MUL;
    Type type1, type2;
    Method meth;
    if (addOrMul)
      {
        if (inRange(arg1, Integer.MIN_VALUE, Integer.MAX_VALUE))
          kind1 = Arithmetic.INT_CODE;
        if (inRange(arg2, Integer.MIN_VALUE, Integer.MAX_VALUE))
          kind2 = Arithmetic.INT_CODE;
        swap = kind1 == Arithmetic.INT_CODE && kind2 != Arithmetic.INT_CODE;
        if (swap && ! (arg1.side_effects() && arg2.side_effects()))
          return compileIntNum(arg2, arg1, kind2, kind1, comp);
        type1 = kind1 == Arithmetic.INT_CODE ? Type.intType :  Arithmetic.typeIntNum;
        type2 = kind2 == Arithmetic.INT_CODE ? Type.intType :  Arithmetic.typeIntNum;
      }
    else
      {
        type1 = type2 = Arithmetic.typeIntNum;
        swap = false;
      }
    arg1.compile(comp, type1);
    arg2.compile(comp, type2);
    CodeAttr code = comp.getCode();
    if (swap)
      {
        code.emitSwap();
        type1 = Arithmetic.typeIntNum;
        type2 = LangPrimType.intType;
      }
    String mname;
    switch (op)
      {
      case ADD: mname = "add";  break;
      case SUB: mname = "sub";  break;
      case MUL: mname = "times";  break;
      default: throw new Error();
      }
    meth = Arithmetic.typeIntNum.getMethod(mname, new Type[] { type1, type2 });
    code.emitInvokeStatic(meth);
    return true;
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
    switch (op)
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
    switch (op)
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
