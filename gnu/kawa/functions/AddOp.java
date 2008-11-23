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

public class AddOp extends ArithOp
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    super(name);
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
    int code = Arithmetic.classifyValue(arg1);
    switch (code)
      {
      case Arithmetic.INT_CODE:
	return new Integer(- Arithmetic.asInt(arg1));
      case Arithmetic.LONG_CODE:
	return new Long(- Arithmetic.asLong(arg1));
      case Arithmetic.BIGINTEGER_CODE:
	return Arithmetic.asBigInteger(arg1).negate();
      case Arithmetic.INTNUM_CODE:
	return IntNum.neg(Arithmetic.asIntNum(arg1));
      case Arithmetic.BIGDECIMAL_CODE:
	return Arithmetic.asBigDecimal(arg1).negate();
      case Arithmetic.RATNUM_CODE:
	return RatNum.neg(Arithmetic.asRatNum(arg1));
      case Arithmetic.FLOAT_CODE:
	return new Float(- Arithmetic.asFloat(arg1));
      case Arithmetic.DOUBLE_CODE:
	return new Double(- Arithmetic.asDouble(arg1));
      case Arithmetic.FLONUM_CODE:
	return new DFloNum(- Arithmetic.asDouble(arg1));
      default:
        return Arithmetic.asNumeric(arg1).neg();
      }

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
          prev = ((CanInline) proc).inline(next, walker, true);
        else
          prev = next;
      }
    return prev;
  }

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    exp.walkArgs(walker, argsInlined);
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
	return primInline(primitiveOpcode(), exp);
      }
    return exp;
  }

  public int primitiveOpcode ()
  {
    return plusOrMinus > 0 ? 96 /* iadd */ : 100 /* isub */;
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

}
