// Copyright (c) 2000, 2001, 2003, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangPrimType;

/**
 * Implement the Scheme standard functions "+" and "-".
 * @author Per Bothner
 */

public class AddOp extends ProcedureN implements CanInline, Inlineable
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    setName(name);
    this.plusOrMinus = plusOrMinus;
  }

  public static final AddOp $Pl = new AddOp("+", 1);
  public static final AddOp $Mn = new AddOp("-", -1);

  public static Object apply2(int plusOrMinus, Object arg1, Object arg2)
  {
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    /*
    if (code1 < 0 || code2 < 0)
    throw new ClasscastException(); // FIXME
    */
    int code = code1 < code2 ? code2 : code1;
    switch (code)
      {
      case Arithmetic.INT_CODE:
	int i1 = Arithmetic.asInt(arg1);
	int i2 = Arithmetic.asInt(arg2);
	return new Integer(plusOrMinus > 0 ? i1 + i2 : i1 - i2);
      case Arithmetic.LONG_CODE:
	long l1 = Arithmetic.asLong(arg1);
	long l2 = Arithmetic.asLong(arg2);
	return new Long(plusOrMinus > 0 ? l1 + l2 : l1 - l2);
      case Arithmetic.BIGINTEGER_CODE:
	BigInteger bi1 = Arithmetic.asBigInteger(arg1);
	BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	return plusOrMinus > 0 ? bi1.add(bi2) : bi1.subtract(bi2);
      case Arithmetic.INTNUM_CODE:
	return IntNum.add(Arithmetic.asIntNum(arg1), Arithmetic.asIntNum(arg2),
			  plusOrMinus);
      case Arithmetic.BIGDECIMAL_CODE:
	BigDecimal bd1 = Arithmetic.asBigDecimal(arg1);
	BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	return plusOrMinus > 0 ? bd1.add(bd2) : bd1.subtract(bd2);
      case Arithmetic.RATNUM_CODE:
	return RatNum.add(Arithmetic.asRatNum(arg1), Arithmetic.asRatNum(arg2),
			  plusOrMinus);
      case Arithmetic.FLOAT_CODE:
	float f1 = Arithmetic.asFloat(arg1);
	float f2 = Arithmetic.asFloat(arg2);
	return new Float(plusOrMinus > 0 ? f1 + f2 : f1 - f2);
      case Arithmetic.DOUBLE_CODE:
	double d1 = Arithmetic.asDouble(arg1);
	double d2 = Arithmetic.asDouble(arg2);
	return new Double(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      case Arithmetic.FLONUM_CODE:
	d1 = Arithmetic.asDouble(arg1);
	d2 = Arithmetic.asDouble(arg2);
	return new DFloNum(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      default:
	Numeric num1 = Arithmetic.asNumeric(arg1);
	Numeric num2 = Arithmetic.asNumeric(arg2);
	return num1.add(num2, plusOrMinus);
      }
  }

  public static Object $Pl(Object arg1, Object arg2)
  {
    return apply2(1, arg1, arg2);
  }

  public static Object $Mn(Object arg1, Object arg2)
  {
    return apply2(-1, arg1, arg2);
  }

  public static Object $Mn(Object arg1)
  {
    return ((Numeric) arg1).neg();
  }

  public static Object $Pl$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(1, apply2(1,apply2(1, arg1, arg2), arg3), rest);
  }

  public static Object $Mn$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(-1, apply2(-1,apply2(-1, arg1, arg2), arg3), rest);
  }

  public static Object applyN(int plusOrMinus, Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Object result = args[0];
    if (len == 1 && plusOrMinus < 0)
      return $Mn(result);
    for (int i = 1; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public static Object applyN(int plusOrMinus, Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return applyN(plusOrMinus, args);
  }

  /** Convert (PROC A B C) to (PROC (PROC A B) C) etc. */

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
        if (proc instanceof CanInline)
          prev = ((CanInline) proc).inline(next, walker);
        else
          prev = next;
      }
    return prev;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
    // Inlining may yield PrimProcedure instructions of bytecode instructions
    // which we don't know how to interpret (yet).
    if (! walker.getCompilation().mustCompile)
      return exp;
    Expression folded = exp.inlineIfConstant(this, walker);
    if (folded != exp)
      return folded;
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(this, exp.getFunction(), args, walker);
    if (args.length == 1 && plusOrMinus < 0)
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
	return primInline(plusOrMinus > 0 ? 96 /* iadd */ : 100 /* isub */,
			  exp);
      }
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

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int len = args.length;
    if (len == 0)
      {
	comp.compileConstant(IntNum.zero(), target);
	return;
      }
    if (len == 1 || target instanceof IgnoreTarget)
      {
	// FIXME implement optimization for unary
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Type type = getReturnType(args);
    int kind = Arithmetic.classifyType(type);
    if (kind == 0)
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
	// FIXME would be nice to use iinc when appropriate!
	// We would need to use a special LocalVariableTarget,
	// created by SetExp when dest is a local variable.
	// Then if len==2 && ptype==LangPrimType.intType
	// && target instanceof LocalVariableTarget
	// && one arg is QuoteExp && other arg is same local as target
	// => then emit iinc.
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
            if (plusOrMinus > 0)
              code.emitAdd((PrimType) wtype);
            else
              code.emitSub((PrimType) wtype);
            break;
          }
      }
    target.compileFromStack(comp, wtype);
  }

  /** Classify an expression according to its numeric type.
   * kind==0:  not a number.
   * kind==1:  a non-real number
   * kind==2:  real number
   * kind==3:  floating-point
   * kind==4:  exact integer
   */
  public static int classify (Type type)
  {
    int kind = 0;
    if (type instanceof PrimType)
      {
	char sig = type.getSignature().charAt(0);
	if (sig == 'V' || sig == 'Z' || sig == 'C')
	  return 0;
	else if (sig == 'D' || sig == 'F')
	  return 3;
	else
	  return 4;
      }
    else if (type.isSubtype(Arithmetic.typeIntNum))
      return 4;
    else if (type.isSubtype(Arithmetic.typeDFloNum))
      return 3;
    else if (type.isSubtype(Arithmetic.typeRealNum))
      return 2;
    else if (type.isSubtype(Arithmetic.typeNumeric))
      return 1;
    else
      return 0;
  }

  public Type getReturnType (Expression[] args)
  {
    int len = args.length;
    if (len == 0)
      return Arithmetic.typeIntNum;
    Type type = Type.pointer_type;
    int kindr = 0;
    for (int i = 0;  i < len;  i++)
      {
	Expression arg = args[i];
	int kind = Arithmetic.classifyType(arg.getType());

	if (i == 0 || kind == 0 || kind > kindr)
	  kindr = kind;
      }
    return Arithmetic.kindType(kindr);
  }
}
